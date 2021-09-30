Tidy Data
================

## R Markdown

## pivot longer

``` r
pulse_df <- read_sas("data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names()

pulse_df
```

    ## # A tibble: 1,087 × 7
    ##       id   age sex    bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##    <dbl> <dbl> <chr>         <dbl>         <dbl>         <dbl>         <dbl>
    ##  1 10003  48.0 male              7             1             2             0
    ##  2 10015  72.5 male              6            NA            NA            NA
    ##  3 10022  58.5 male             14             3             8            NA
    ##  4 10026  72.7 male             20             6            18            16
    ##  5 10035  60.4 male              4             0             1             2
    ##  6 10050  84.7 male              2            10            12             8
    ##  7 10078  31.3 male              4             0            NA            NA
    ##  8 10088  56.9 male              5            NA             0             2
    ##  9 10091  76.0 male              0             3             4             0
    ## 10 10092  74.2 female           10             2            11             6
    ## # … with 1,077 more rows

``` r
pulse_tidy <-
  pulse_df %>%
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to = "bdi"
  ) %>%
  mutate(
    visit = replace(visit, visit == "bl", "00m"),
    visit = factor(visit)
  )
pulse_tidy
```

    ## # A tibble: 4,348 × 5
    ##       id   age sex   visit   bdi
    ##    <dbl> <dbl> <chr> <fct> <dbl>
    ##  1 10003  48.0 male  00m       7
    ##  2 10003  48.0 male  01m       1
    ##  3 10003  48.0 male  06m       2
    ##  4 10003  48.0 male  12m       0
    ##  5 10015  72.5 male  00m       6
    ##  6 10015  72.5 male  01m      NA
    ##  7 10015  72.5 male  06m      NA
    ##  8 10015  72.5 male  12m      NA
    ##  9 10022  58.5 male  00m      14
    ## 10 10022  58.5 male  01m       3
    ## # … with 4,338 more rows

## pivot\_wider

make up a result data table

``` r
analysis_df <-
  tibble(
    group = c("treatment", "treatment", "control", "control"),
    time = c("a","b","a","b"),
    group_mean = c(4, 8, 3, 6)
  )
analysis_df %>%
  pivot_wider(
    names_from = "time",
    values_from = "group_mean"
  ) %>%
  knitr::kable()
```

| group     |   a |   b |
|:----------|----:|----:|
| treatment |   4 |   8 |
| control   |   3 |   6 |

## Bind rows

``` r
# Load individual dataset
fellowship_ring = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") %>%
  mutate(movie = "fellowship_ring")

two_towers = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") %>%
  mutate(movie = "two_towers")

return_king = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") %>%
  mutate(movie = "return_king")

# bind dataframes
lotr_df <-
  bind_rows(fellowship_ring, two_towers, return_king) %>%
  janitor::clean_names() %>%
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words"
  ) %>%
  relocate(movie)
# or select(movie, everthing()), select the data.frame by movie names
lotr_df
```

    ## # A tibble: 18 × 4
    ##    movie           race   sex    words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring Elf    female  1229
    ##  2 fellowship_ring Elf    male     971
    ##  3 fellowship_ring Hobbit female    14
    ##  4 fellowship_ring Hobbit male    3644
    ##  5 fellowship_ring Man    female     0
    ##  6 fellowship_ring Man    male    1995
    ##  7 two_towers      Elf    female   331
    ##  8 two_towers      Elf    male     513
    ##  9 two_towers      Hobbit female     0
    ## 10 two_towers      Hobbit male    2463
    ## 11 two_towers      Man    female   401
    ## 12 two_towers      Man    male    3589
    ## 13 return_king     Elf    female   183
    ## 14 return_king     Elf    male     510
    ## 15 return_king     Hobbit female     2
    ## 16 return_king     Hobbit male    2673
    ## 17 return_king     Man    female   268
    ## 18 return_king     Man    male    2459

``` r
# Relocate cols
```

-   `rbind`, `cbind`? Never use this Look at FAS data

``` r
litters_df <- 
  read_csv("data/FAS_litters.csv") %>%
  janitor::clean_names() %>%
  separate(group, into = c("dose", "day_of_tx"), sep = 3) %>%
  relocate(litter_number) %>%
  mutate(dose = str_to_lower(dose))
```

    ## Rows: 49 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_df
```

    ## # A tibble: 49 × 9
    ##    litter_number   dose  day_of_tx gd0_weight gd18_weight gd_of_birth
    ##    <chr>           <chr> <chr>          <dbl>       <dbl>       <dbl>
    ##  1 #85             con   7               19.7        34.7          20
    ##  2 #1/2/95/2       con   7               27          42            19
    ##  3 #5/5/3/83/3-3   con   7               26          41.4          19
    ##  4 #5/4/2/95/2     con   7               28.5        44.1          19
    ##  5 #4/2/95/3-3     con   7               NA          NA            20
    ##  6 #2/2/95/3-2     con   7               NA          NA            20
    ##  7 #1/5/3/83/3-3/2 con   7               NA          NA            20
    ##  8 #3/83/3-3       con   8               NA          NA            20
    ##  9 #2/95/3         con   8               NA          NA            20
    ## 10 #3/5/2/2/95     con   8               28.5        NA            20
    ## # … with 39 more rows, and 3 more variables: pups_born_alive <dbl>,
    ## #   pups_dead_birth <dbl>, pups_survive <dbl>

``` r
pups_df <-
  read_csv("data/FAS_pups.csv") %>%
  janitor::clean_names() %>%
  mutate(sex = recode(sex, `1` = "male", `2` = "female"))
```

    ## Rows: 313 Columns: 6

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pups_df
```

    ## # A tibble: 313 × 6
    ##    litter_number sex   pd_ears pd_eyes pd_pivot pd_walk
    ##    <chr>         <chr>   <dbl>   <dbl>    <dbl>   <dbl>
    ##  1 #85           male        4      13        7      11
    ##  2 #85           male        4      13        7      12
    ##  3 #1/2/95/2     male        5      13        7       9
    ##  4 #1/2/95/2     male        5      13        8      10
    ##  5 #5/5/3/83/3-3 male        5      13        8      10
    ##  6 #5/5/3/83/3-3 male        5      14        6       9
    ##  7 #5/4/2/95/2   male       NA      14        5       9
    ##  8 #4/2/95/3-3   male        4      13        6       8
    ##  9 #4/2/95/3-3   male        4      13        7       9
    ## 10 #2/2/95/3-2   male        4      NA        8      10
    ## # … with 303 more rows

Let’s join these up

``` r
fas_df <-
  left_join(pups_df, litters_df, by="litter_number") 
fas_df
```

    ## # A tibble: 313 × 14
    ##    litter_number sex   pd_ears pd_eyes pd_pivot pd_walk dose  day_of_tx
    ##    <chr>         <chr>   <dbl>   <dbl>    <dbl>   <dbl> <chr> <chr>    
    ##  1 #85           male        4      13        7      11 con   7        
    ##  2 #85           male        4      13        7      12 con   7        
    ##  3 #1/2/95/2     male        5      13        7       9 con   7        
    ##  4 #1/2/95/2     male        5      13        8      10 con   7        
    ##  5 #5/5/3/83/3-3 male        5      13        8      10 con   7        
    ##  6 #5/5/3/83/3-3 male        5      14        6       9 con   7        
    ##  7 #5/4/2/95/2   male       NA      14        5       9 con   7        
    ##  8 #4/2/95/3-3   male        4      13        6       8 con   7        
    ##  9 #4/2/95/3-3   male        4      13        7       9 con   7        
    ## 10 #2/2/95/3-2   male        4      NA        8      10 con   7        
    ## # … with 303 more rows, and 6 more variables: gd0_weight <dbl>,
    ## #   gd18_weight <dbl>, gd_of_birth <dbl>, pups_born_alive <dbl>,
    ## #   pups_dead_birth <dbl>, pups_survive <dbl>
