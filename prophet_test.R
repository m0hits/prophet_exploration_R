#### Session Setup ----
rm(list = ls())
gc()
set.seed(786)
Time = Sys.time()

#### Packages ----
list.of.packages <- c("tidyverse", 
                      "forecast", 
                      "purrr", 
                      "broom", 
                      "readxl", 
                      "writexl", 
                      "lubridate", 
                      "prophet",
                      "dygraphs")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"] )]
if(length(new.packages)) install.packages(new.packages) 

for(i in list.of.packages){
    library(i, character.only = TRUE)
}


#### Input variables ----
test_period = 5



#### Read Input data ----
raw <- read_xlsx("Raw_Data.xlsx")

#### Data Processing ----
# raw <- raw %>% 
#   spread(key = Period, value = Sales, fill = 0) %>% 
#   gather(key = "Period", value = "Sales", 2:ncol(.))

raw_nest <- raw %>% 
    group_by(Id) %>% 
    nest(-Id)


#### Generate ts objects ----
ts_create <- function(x){
    ts(x$Sales,
       start = c(str_sub(x$Period[[1]], 1, 4) %>% as.numeric(), str_sub(x$Period[[1]], 5, 6) %>% as.numeric()),
       frequency = 12)
}

raw_nest <- raw_nest %>% 
    mutate(data_ts = map(data, ts_create))


#### Model Definition ----
f_prophet <- function(x, h){
    dat = data.frame(ds = as_date(time(x)), y = as.matrix(x))
    prophet(dat)
}

#### Apply model ----

raw_nest <- raw_nest %>% 
    mutate(prophet_fit = map(data_ts, f_prophet))

#### Forecasting using the model ----
f_predict <- function(x){
    df <- make_future_dataframe(x, 24, freq = 'month', include_history = T) 
    predict(x, df)
}

raw_nest <- raw_nest %>% 
    mutate(forecast = map(prophet_fit, f_predict))

#### Visualization ----
dyplot.prophet(raw_nest$prophet_fit[[1]], raw_nest$forecast[[1]])
dyplot.prophet(raw_nest$prophet_fit[[2]], raw_nest$forecast[[2]])
dyplot.prophet(raw_nest$prophet_fit[[3]], raw_nest$forecast[[3]])
