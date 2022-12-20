library(quantmod)
library(tseries)
library(rugarch)
library(forecast)

# get data
gold_price = getSymbols("GC=F", auto.assign = FALSE)
gold_return = dailyReturn(gold_price)[-1]

# initial analysis
gold.diff = diff(gold_return)[-1]
dates = index(gold_return)
par(mfrow = c(2,2))
plot(dates, gold_return, type = "l", main = "Gold Return")
plot(dates[-1], gold.diff, type = "l", main = "Changes in Gold Return")
acf(gold_return)
acf(gold.diff)
# test of stationarity
adf.test(gold_return)
kpss.test(gold_return)
adf.test(gold.diff)
kpss.test(gold.diff)
# auto arima
gold.arima = auto.arima(gold_return)
gold.arima
par(mfrow = c(1,2))
acf(gold.arima$residuals)
acf(gold.arima$residuals^2)

# GARCH(1,1)
garch.t = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                     variance.model = list(garchOrder = c(1,1)),
                     distribution.model = "std")
gold.garch.t = ugarchfit(data = gold_return, spec = garch.t)
show(gold.garch.t)
par(mfrow = c(2,2))
plot(gold.garch.t, which = 8)
plot(gold.garch.t, which = 9)
plot(gold.garch.t, which = 10)
plot(gold.garch.t, which = 11)

# IGARCH(1,1)
igarch.t = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                     variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
                     distribution.model = "std")
gold.igarch.t = ugarchfit(data = gold_return, spec = igarch.t)
show(gold.igarch.t)
par(mfrow = c(2,2))
plot(gold.igarch.t, which = 8)
plot(gold.igarch.t, which = 9)
plot(gold.igarch.t, which = 10)
plot(gold.igarch.t, which = 11)

# FIGARCH(1,1)
figarch.t = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "fiGARCH", submodel = "GARCH",
                                            garchOrder = c(1,1)),
                      distribution.model = "std")
gold.figarch.t = ugarchfit(data = gold_return, spec = figarch.t)
show(gold.figarch.t)
par(mfrow = c(2,2))
plot(gold.figarch.t, which = 8)
plot(gold.figarch.t, which = 9)
plot(gold.figarch.t, which = 10)
plot(gold.figarch.t, which = 11)

# return forecasting
test = gold_return["2021-01-01/"]
n_forecast = length(test)
spec = getspec(gold.garch.t)
setfixed(spec) = as.list(coef(gold.garch.t))
pred_garch = ugarchforecast(spec, data = gold_return, n.ahead = 1, 
                            n.roll = n_forecast - 1, out.sample = n_forecast)
mae_garch = sum(abs(as.numeric(fitted(pred_garch)) - test))
rmse_garch = sqrt(sum((as.numeric(fitted(pred_garch)) - test)^2)/n_forecast)

spec = getspec(gold.igarch.t)
setfixed(spec) = as.list(coef(gold.igarch.t))
pred_igarch = ugarchforecast(spec, data = gold_return, n.ahead = 1, 
                            n.roll = n_forecast - 1, out.sample = n_forecast)
mae_igarch = sum(abs(as.numeric(fitted(pred_igarch)) - test))
rmse_igarch = sqrt(sum((as.numeric(fitted(pred_igarch)) - test)^2)/n_forecast)

spec = getspec(gold.figarch.t)
setfixed(spec) = as.list(coef(gold.figarch.t))
pred_figarch = ugarchforecast(spec, data = gold_return, n.ahead = 1, 
                            n.roll = n_forecast - 1, out.sample = n_forecast)
mae_figarch = sum(abs(as.numeric(fitted(pred_figarch)) - test))
rmse_figarch = sqrt(sum((as.numeric(fitted(pred_figarch)) - test)^2)/n_forecast)

# volatility forecasting plots
par(mfrow = c(1,1))
plot(pred_garch, which = 4)
plot(pred_igarch, which = 4)
plot(pred_figarch, which = 4)

