url <- glue::glue(
  "https://api.ess.sikt.no/v1/data/dataFile/10.21338/ess11e04_2?userId={Sys.getenv('ESS_USERID')}"
)

ess_in <- arrow::read_parquet(url)

country_coding <-
  tibble::tribble(
    ~Code , ~Name                ,
    'AL'  , 'Albania'            ,
    'AT'  , 'Austria'            ,
    'BE'  , 'Belgium'            ,
    'BG'  , 'Bulgaria'           ,
    'CH'  , 'Switzerland'        ,
    'CY'  , 'Cyprus'             ,
    'CZ'  , 'Czechia'            ,
    'DE'  , 'Germany'            ,
    'DK'  , 'Denmark'            ,
    'EE'  , 'Estonia'            ,
    'ES'  , 'Spain'              ,
    'FI'  , 'Finland'            ,
    'FR'  , 'France'             ,
    'GB'  , 'United Kingdom'     ,
    'GE'  , 'Georgia'            ,
    'GR'  , 'Greece'             ,
    'HR'  , 'Croatia'            ,
    'HU'  , 'Hungary'            ,
    'IE'  , 'Ireland'            ,
    'IS'  , 'Iceland'            ,
    'IL'  , 'Israel'             ,
    'IT'  , 'Italy'              ,
    'LT'  , 'Lithuania'          ,
    'LU'  , 'Luxembourg'         ,
    'LV'  , 'Latvia'             ,
    'ME'  , 'Montenegro'         ,
    'MK'  , 'North Macedonia'    ,
    'NL'  , 'Netherlands'        ,
    'NO'  , 'Norway'             ,
    'PL'  , 'Poland'             ,
    'PT'  , 'Portugal'           ,
    'RO'  , 'Romania'            ,
    'RS'  , 'Serbia'             ,
    'RU'  , 'Russian Federation' ,
    'SE'  , 'Sweden'             ,
    'SI'  , 'Slovenia'           ,
    'SK'  , 'Slovakia'           ,
    'TR'  , 'Turkey'             ,
    'UA'  , 'Ukraine'            ,
    'XK'  , 'Kosovo'
  )

ess_der <-
  ess_in |>
  dplyr::select(
    psu,
    cntry,
    stratum,
    anweight,
    happy,
    wrhpp,
    nwspol,
    volunfp,
    netusoft,
    netustm,
    agegroup
  ) |>
  dplyr::mutate(
    Happy_Freq = dplyr::if_else(wrhpp %in% 7:9, NA, wrhpp),
    Happy = dplyr::if_else(happy %in% c(77, 88, 99), NA, happy),
    News = dplyr::if_else(nwspol %in% c(7777, 8888, 9999), NA, nwspol),
    Country = dplyr::recode_values(
      cntry,
      from = country_coding$Code,
      to = country_coding$Name
    ),
    Volunteer_12 = if_else(volunfp %in% 7:9, NA, volunfp),
    Volunteer_01 = case_when(
      volunfp == 1 ~ 1,
      volunfp == 2 ~ 0,
      TRUE ~ NA
    ),
    Volunteer_Factor = case_when(
      volunfp == 1 ~ "Yes",
      volunfp == 2 ~ "No",
      TRUE ~ NA
    ) |>
      factor(levels = c("Yes", "No")),
    NetUsage = case_when(
      netusoft %in% 1:3 ~ 0,
      netustm %in% c(6666, 7777, 8888, 9999) ~ NA,
      TRUE ~ netustm
    ),
    NetFreq = recode_values(
      netusoft,
      1 ~ "Never",
      2 ~ "Only occasionally",
      3 ~ "A few times a week",
      4 ~ "Most days",
      5 ~ "Every day"
    ) |>
      fct_reorder(netusoft),
    AgeGroup = recode_values(
      agegroup,
      1 ~ '15-24',
      2 ~ '25-34',
      3 ~ '35-44',
      4 ~ '45-54',
      5 ~ '55-64',
      6 ~ '65-74',
      7 ~ '75+'
    ) |>
      fct_reorder(agegroup)
  )

ess_des <-
  ess_der |>
  srvyr::as_survey_design(
    ids = psu,
    strata = stratum,
    weights = anweight,
    nest = TRUE
  )

rm(url)
