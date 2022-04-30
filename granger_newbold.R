


library(tidyverse)
library(forecast)

set.seed(1990)
options(scipen = 999)

# rw without drift 

rw1 <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 0, sd = 1)
rw2 <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 0, sd = 1)

tseries::seqplot.ts(rw1, rw2)

# rw with drift 

rw_drift1 <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 1, sd = 1)
rw_drift2 <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 1, sd = 1)

tseries::seqplot.ts(rw_drift1, rw_drift2)

# regressions

reg_without_drift <- lm(rw1 ~ rw2)
summary(reg_without_drift)


reg_with_drift <- lm(rw_drift1 ~ rw_drift2)
summary(reg_with_drift)



# simulation 

p_vals_no_drift <- numeric(200)
p_vals_with_drift <- numeric(200)

for (i in 1:200) {
  
  rw1 <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 0, sd = 1)
  rw2 <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 0, sd = 1)
  
  rw_drift1 <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 1, sd = 1)
  rw_drift2 <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 1, sd = 1)
  
  p_vals_no_drift[i] <- summary(lm(rw1 ~ rw2))$coefficients[2,4]
  p_vals_with_drift[i] <- summary(lm(rw_drift1 ~ rw_drift2))$coefficients[2,4]
  
}

mean(p_vals_no_drift)
mean(p_vals_with_drift)

sum(p_vals_no_drift < 0.05) / 200
sum(p_vals_with_drift < 0.05) / 200
