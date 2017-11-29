library(data.table)
library(readxl)
library(lubridate)

# IMPORT STATIONS DATA ####
dir.data <- c("/Users/tommasocondulmari/Google Drive/GROUP PROJECT/R/data.sets/workgroup data/")
setwd(dir.data)

files <- list.files(path = dir.data, pattern = ".csv") # creates a list with all file names
raw_data_list <- lapply(files, fread, sep=",") # reads all the data sets from the files.list

#add year and month column in all elements of the list, as first two columns
add_year_month <- function(x){  
  count <- 1
  for (i in 2011:2016){
    for (j in 1:12){
      x[[count]][, year := i]
      x[[count]][, month := j]
      setcolorder(x[[count]], c('year', 'month', names(x[[count]])[1:(length(x[[count]])-2)]))
      count <- count + 1
    }
  }
}

add_year_month(raw_data_list)
raw_data_list[[72]] # check
str(raw_data_list[[72]])

add_date_list <- function(x){
  for (i in 1:length(x)){
    x[[i]][, date := as.Date(paste(year, month, day, sep = "-"))]
    x[[i]][, c('year', 'month', 'day') := NULL]
    setcolorder(x[[i]], c('date', names(x[[i]])[1:(length(x[[i]])-1)]))
  }
  return(x)
}

data_list <- add_date_list(raw_data_list)
data_list[[72]] # check
str(data_list[[72]])


# aggregate al the data in one data.table 
# data <- rbindlist( temp )

# IMPORT WEATHER DATA ####
weather <- readxl::read_xlsx("/Users/tommasocondulmari/Google Drive/GROUP PROJECT/R/data.sets/weather.xlsx")
weather <- as.data.table(weather) # convert to data.table
head(weather)
str(weather)

# add date variable as first row formatted as .date
weather[, date := as.Date(paste(year, month, day, sep = "-"))]
weather[, c('year', 'month', 'day') := NULL] # drop single (useless) variables 
setcolorder(weather, c('date', names(weather[,1:(length(weather)-1)])))
weather

# IMPORT SYMBOLS TABLE ####
symbols <- readxl::read_xlsx("/Users/tommasocondulmari/Google Drive/GROUP PROJECT/R/data.sets/Workbook2.xlsx")
symbols <- as.data.frame(symbols)
symbols[,1] <- as.numeric(symbols[,1])
str(symbols)

# MERGE data_list with symbols ####
symbols_imp <- function(dt){
  for (i in 1:length(dt)){
    dt[[i]] <- merge(dt[[i]], symbols, by.x = 'parameter', by.y = 'ID')
    dt[[i]][, c("name", "symbol", "parameter") := NULL]
    setcolorder(dt[[i]], c("date", "hour", "station","s_symbol", "value", "s_name", "units"))
  }
  return(dt)
}

data_list_symb <- symbols_imp(data_list)
data_list_symb[[1]] # check

# MERGE data_list with weather ####
weather_imp <- function(dt){
  for (i in 1:length(dt)){
    dt[[i]] <- merge(dt[[i]], weather, by = c("date"))
  }
  return(dt)
}

data_list_weather <- weather_imp(data_list)
data_list_weather[[1]] # check

# Calculate mean values per MM ####
monthly_avg <- function(dt){
  dl <- list()
  for (i in 1:length(dt)){
    dl[[i]] <- dt[[i]][, .(avg_value = mean(value)),
                                  by = .(year(date), month(date), s_symbol)]
  }
  return(dl)
}

data_month_avg <- monthly_avg(data_list_symb)
data_month_avg[[25]] # check

# Calculate mean values per DD ####
daily_avg <- function(dt){
  dl <- list()
  for (i in 1:length(dt)){
    dl[[i]] <- dt[[i]][, .(avg_value = mean(value)),
                                   by = .(day(date), s_symbol)]
  }
  return(dl)
}

data_day_avg <- daily_avg(data_list_symb)
data_day_avg[[72]] #check 

# CREATE data_list_par with parameters as variables (columns) ####
data_parameters <- function(x){
  dl <- list()
  for (i in 1:length(x)){
    setkey(x[[i]], s_symbol)
    dl[[i]] <- cbind(x[[i]]["so2", .(day, so2 = avg_value)],
                                x[[i]]["co", .(co = avg_value)],
                                x[[i]]["no", .(no = avg_value)],
                                x[[i]]["no2", .(no2 = avg_value)],
                                x[[i]]["pm_2.5", .(pm_2.5 = avg_value)],
                                x[[i]]["pm_10", .(pm_10 = avg_value)],
                                x[[i]]["nox", .(nox = avg_value)],
                                x[[i]]["o3", .(o3 = avg_value)],
                                x[[i]]["tol", .(tol = avg_value)],
                                x[[i]]["ben", .(ben = avg_value)],
                                x[[i]]["ebe", .(ebe = avg_value)],
                                x[[i]]["mxy", .(mxy = avg_value)],
                                x[[i]]["pxy", .(pxy = avg_value)],
                                x[[i]]["oxy", .(oxy = avg_value)],
                                x[[i]]["tch", .(tch = avg_value)],
                                x[[i]]["ch4", .(ch4 = avg_value)],
                                x[[i]]["nmhc", .(nmhc = avg_value)]
                                )
  }
  return(dl)
}


data_list_par <- data_parameters(data_day_avg)
data_list_par[[72]] # check

# # it's possible to use "dcast" instead of the function above, but elements of the list 
# # would have differetn dimension because not all of them register the same parametrs
# data_day_avg[[72]]
# i <- dcast(data_day_avg[[72]], day ~ s_symbol, value.var = "avg_value")

# add date to list
add_date <- function(x){
  count <- 1
  for (i in 2011:2016){
    for (j in 1:12){
      x[[count]][, date := as.Date(paste0(i, "-", j, "-", day))]
      count <- count + 1
    }
  }
  return(x)
}

data_list_par <- add_date(data_list_par)
data_list_par[[1]] # check
str(data_list_par[[72]])

order_date <- function(x){
  for (i in 1:length(x)){
    x[[i]][, day := NULL]
    setcolorder(x[[i]], c('date', names(x[[i]])[1:(length(x[[i]])-1)]))
  }
  return(x)
}

data_list_par <- order_date(data_list_par)
data_list_par[[72]] # check
str(data_list_par[[72]])

# CREATE data_list_clean ####
round_parameters <- function(x){
  for(i in 1:length(x)){
    x[[i]][,2:18] <- round(x[[i]][,2:18], digits = 2)
    #for (j in 2:ncol(x[[i]])){
    #x[[i]][,j] <- round(x[[i]][,j], digits = 2)
  }
  return(x)
}

data_list_clean <- round_parameters(data_list_par)
data_list_clean[[72]]

# Convert date to POSIXct ####
# convert_date <- function(dl){
#   for(i in 1:length(dl)){
#     dl[[i]] <- as.data.frame(dl[[i]])
#     dl[[i]][, "date"] <- as.Date.character(dl[[i]][, 'date'])
#     dl[[i]][, "date"] <- as.POSIXct(dl[[i]][, 'date'])
#     dl[[i]] <- as.data.table(dl[[i]])
#     dl[[i]][, "day" := NULL]
#     setcolorder(dl[[i]], c('date', names(dl[[i]])[1:(length(dl[[i]])-1)]))
#   }
#   return(dl)
# }
# 
# data_list_par <- convert_date(data_list_par)
# class(data_list_par[[72]][, "date"])
