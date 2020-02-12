#Name: Michael Lee
#Date: 12/02/20
#Title: Weather Canada Workshop

#Index:
# 1. Installation and Paths
# 2. Station Search
# 3. Plots and Figures
# 4. Correlation Tests
# 5. Tables


#--------------------------------- 1. Installation and Paths -------------------
install.packages("weathercan")
library(weathercan)
head(stations)
View(stations)

dir.create("CSV")
figurepath <- paste(getwd(), "/", "Figures", sep = "")
csvpath <- paste(getwd(), "/", "CSV", "/", sep = "")

write.csv(pemb, paste(csvpath, "lil"))
#--------------------------------- 2. Station Search ---------------------------

#We search for stations in each area that show daily intervals
Squamish <- stations_search("Squamish", interval = "day")
Vancouver <- stations_search("Vancouver", interval = "day")
Lillooet <- stations_search("Lillooet", interval = "day")
Pemberton <- stations_search("Pemberton", interval = "day")

#We choose stations based on the time period they were active, on a daily 
#interval
squamairport <- weather_dl(station_ids = 336,
                           interval = "day",
                           start = "2019-01-01", 
                           end = "2019-12-31")

wvaut <- weather_dl(station_ids = 6833,
                    interval = "day",
                    start = "2019-01-01", 
                    end = "2019-12-31")

lil <- weather_dl(station_ids = 27388,
                  interval = "day",
                  start = "2019-01-01", 
                  end = "2019-12-31")

pemb <- weather_dl(station_ids = 536,
                   interval = "day",
                   start = "2019-01-01", 
                   end = "2019-12-31")

#--------------------------------- 3. Plots and Figures ------------------------

#We can plot the max temperature on a daily interval over a year

#This shows the data points for West Van
plot(wvaut$max_temp,
     main = "Max Temperature in West Van Over a Year",
     xlab = "Day",
     ylab = "Max Temperature",
     col = "black",
     cex.main = 1)
#This code shows the distribution
plot(wvaut$max_temp,
     main = "Max Temperature in West Van Over a Year",
     xlab = "Day",
     ylab = "Max Temperature",
     col = "white",
     cex.main = 1)
lines(wvaut$max_temp)

#Data points for Lillooet
plot(lil$max_temp,
     main = "Max Temperature in Lillooet Over a Year",
     xlab = "Day",
     ylab = "Max Temperature",
     col = "black",
     cex.main = 1)
#This shows the distribution
plot(lil$max_temp,
     main = "Max Temperature in Lillooet Over a Year",
     xlab = "Day",
     ylab = "Max Temperature",
     col = "white",
     cex.main = 1)
lines(lil$max_temp)

#Data points for Pemberton
plot(pemb$max_temp,
     main = "Max Temperature in Pemberton Over a Year",
     xlab = "Day",
     ylab = "Max Temperature",
     col = "black",
     cex.main = 1)
#This shows the distribution
plot(pemb$max_temp,
     main = "Max Temperature in Pemberton Over a Year",
     xlab = "Day",
     ylab = "Max Temperature",
     col = "white",
     cex.main = 1)
lines(pemb$max_temp)

#Data points for Squamish
plot(squamairport$max_temp,
     main = "Max Temperature in Squamish Over a Year",
     xlab = "Day",
     ylab = "Max Temperature",
     col = "black",
     cex.main = 1)
#This shows the distribution
plot(squamairport$max_temp,
     main = "Max Temperature in Squamish Over a Year",
     xlab = "Day",
     ylab = "Max Temperature",
     col = "white",
     cex.main = 1)
lines(squamairport$max_temp)

##*** Precipitation Graphs *****************************************************

#We can plot the total precipitation on a daily interval over the course of a
#year. 
plot(squamairport$total_precip, 
     main = "Total Precipitation by Day in Squamish Over a Year",
     xlab = "Day",
     ylab = "Total Precipitation",
     cex.main = 1)
plot(wvaut$total_precip, 
     main = "Total Precipitation by Day in West Van Over a Year",
     xlab = "Day",
     ylab = "Total Precipitation",
     cex.main = 1)
plot(lil$total_precip, 
     main = "Total Precipitation by Day in Lillooet Over a Year",
     xlab = "Day",
     ylab = "Total Precipitation",
     cex.main = 1)
plot(pemb$total_precip, 
     main = "Total Precipitation by Day in Pemberton Over a Year",
     xlab = "Day",
     ylab = "Total Precipitation",
     cex.main = 1)


#--------------------------------- 4. Correlation Tests ------------------------

#We do a correlation test between all the stations. The number of tests goes 
#down due to the fact that the tests were run between all possible groups
#once for max temp and precipitation 

#Correlation tests. We use kendall method for non parametric data(precipitation)
cor.test(wvaut$max_temp, pemb$max_temp)
cor.test(wvaut$max_temp, lil$max_temp)
cor.test(wvaut$max_temp, squamairport$max_temp)
cor.test(wvaut$total_precip, pemb$total_precip, method = "kendall")
cor.test(wvaut$total_precip, lil$total_precip, method = "kendall")
cor.test(wvaut$total_precip, squamairport$total_precip, method = "kendall")

cor.test(squamairport$max_temp, lil$max_temp)
cor.test(squamairport$max_temp, pemb$max_temp)
cor.test(squamairport$total_precip, pemb$total_precip, method = "kendall")
cor.test(squamairport$total_precip, lil$total_precip, method = "kendall")

cor.test(lil$max_temp, pemb$max_temp)
cor.test(lil$total_precip, pemb$total_precip, method = "kendall")


na.wv <- which(is.na(wvaut$max_temp))
na.pemb <- which(is.na(pemb$max_temp))
nawv.pemb <- unique(c(na.wv, na.pemb))

#--------------------------------- 5. Tables -----------------------------------

#We make a table of all the coefficient values relative to each station
Pem <- c(1, 0.97, 0.97, 0.95)
Lil <- c(0.97, 1, 0.94, 0.94)
Squam <- c(0.97, 0.94, 1, 0.97)
WV <- c(0.95, 0.94, 0.97, 1)

MaxTempCor <- cbind(Pem, Lil, Squam, WV)

rownames(MaxTempCor) <- c("Pem", "Lil", "Squam", "WV")
MaxTempCor <- data.frame(MaxTempCor)

#This is how we export the table to 
write.csv(MaxTempCor, paste(csvpath, "Max Temperature Correlation.csv"))

