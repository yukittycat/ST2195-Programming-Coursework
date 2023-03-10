# set working directory
setwd("C:/Users/E5-471G/Documents/Yuki/SIM-UOL/2021-22 Year 3/ST2195 Programming for Data Science/ST2195 Coursework")

# check working directory
getwd()

# load libraries
library(DBI)
library(dplyr)
library(ggplot2)

# install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)

# create SQLite database called airline.db
if (file.exists("airline.db")) 
  file.remove("airline.db") 
conn <- dbConnect(RSQLite::SQLite(), "airline.db")

# load data from csv files
airports <- read.csv(file = "./airports.csv", header = TRUE)
carriers <- read.csv(file = "./carriers.csv", header = TRUE)
planes <- read.csv(file = "./plane-data.csv", header = TRUE)

dbWriteTable(conn, "airports", airports, overwrite = TRUE)
dbWriteTable(conn, "carriers", carriers, overwrite = TRUE)
dbWriteTable(conn, "planes", planes, overwrite = TRUE)

ontime_2007 <- read.csv(file = "./2007.csv.bz2", header = TRUE)
ontime_2008 <- read.csv(file = "./2008.csv.bz2", header = TRUE)

ontime_all <- ontime_2007 %>%
  rbind(ontime_2008)

dbWriteTable(conn, "ontime", ontime_all, overwrite = TRUE)

# remove temporary dataframes
rm(ontime_2007, ontime_2008)

# garbage collection
gc()

dbListTables(conn)
dbListFields(conn, "ontime")

# ==== Q1 ====
# When is the best time of day, day of the week, and time of year to fly to minimise delays?

# get avg departure and arrival delay (in mins) per month, per day, and per distinct scheduled departure time

# query individually
q1_month <- dbGetQuery(conn,
  " SELECT Month AS month, ROUND(AVG(DepDelay),2) AS avg_dep_delay, ROUND(AVG(ArrDelay),2) AS avg_arr_delay
    FROM ontime
    WHERE Cancelled = 0 AND Diverted = 0
    GROUP BY month
    ORDER BY avg_dep_delay, avg_arr_delay") 

q1_day <- dbGetQuery(conn,
  " SELECT DayOfWeek AS day, ROUND(AVG(DepDelay),2) AS avg_dep_delay, ROUND(AVG(ArrDelay),2) AS avg_arr_delay
    FROM ontime
    WHERE Cancelled = 0 AND Diverted = 0
    GROUP BY day
    ORDER BY avg_dep_delay, avg_arr_delay")

q1_time <- dbGetQuery(conn,
  " SELECT DISTINCT(CRSDepTime) AS sch_dep, ROUND(AVG(DepDelay),2) AS avg_dep_delay, ROUND(AVG(ArrDelay),2) AS avg_arr_delay
    FROM ontime
    WHERE Cancelled = 0 AND Diverted = 0
    GROUP BY sch_dep
    ORDER BY avg_dep_delay, avg_arr_delay")

# query for time using all 3 by month and day
q1 <- dbGetQuery(conn,
  " SELECT  DISTINCT(CRSDepTime) AS sch_dep, DayOfWeek AS day, Month AS month,
    ROUND(AVG(DepDelay),2) AS avg_dep_delay, ROUND(AVG(ArrDelay),2) AS avg_arr_delay
    FROM ontime
    WHERE Cancelled = 0 AND Diverted = 0
    GROUP BY month, day
    ORDER BY avg_dep_delay, avg_arr_delay")

# mirror histogram for q1_month
# binwidth = 1 for 1 minute
ggplot() +
  geom_histogram(data = q1_month, binwidth = 1, aes(x = avg_dep_delay, y = ..count.., fill = "cornflowerblue")) +
  geom_histogram(data = q1_month, binwidth = 1, aes(x = avg_arr_delay, y = -..count.., fill = "lightblue")) +
  scale_fill_manual(values = c("lightblue", "cornflowerblue"), labels = c("Departure Delay", "Arrival Delay")) +
  scale_x_continuous(breaks = seq(0,17,1), lim = c(0,17)) +
  labs(fill = "") +
  xlab("Average Delay (in mins)") +
  ylab("Frequency") +
  theme_classic() +
  ggtitle("Distribution of average delay per no. of months")

# had to switch the fill colors in aes to give the desired result 
# else the data will be colored incorrectly (not sure why)
# ie. darker blue (arr) will be above y axis, lighter blue (dep) below -- incorrect 

# mirror histogram for q1_day
ggplot() +
  geom_histogram(data = q1_day, binwidth = 1, aes(x = avg_dep_delay, y = ..count.., fill = "coral")) +
  geom_histogram(data = q1_day, binwidth = 1, aes(x = avg_arr_delay, y = -..count.., fill = "darksalmon")) +
  scale_x_continuous(breaks = seq(0,14,1), lim = c(0,14)) +
  labs(fill = "") +
  xlab("Average Delay (in mins)") +
  ylab("Frequency") +
  theme_classic() +
  scale_fill_manual(values = c("coral", "darksalmon"), labels = c("Departure Delay", "Arrival Delay")) +
  ggtitle("Distribution of average delay by no. of days of the week")

# no issues with fill color for above^

# histogram for q1 
# x axis as time since group by month then day (7 obs each)
# binwidth = 50 for every half hour(30 mins) since numeric
ggplot(q1, aes(x = sch_dep)) +
  geom_histogram(binwidth = 50, fill = "bisque", color = "black") +
  ggtitle("Best time of the day to minimise departure and arrival delays") +
  theme_classic() +
  xlab("Scheduled Departure Time") +
  ylab("Frequency")

# best month is 9th month, September
# best day is the 6th day of the week, Saturday
# best time by month and day is 11.30am

# best month, day, and time is November, Saturday, 11.30am

# ==== Q2 ====
# Do older planes suffer more delays?

# interpret qn as 'how many times did planes manufactured in xx year got delayed (arr/dep)'
# where delay is not cancelled or diverted
# and planes.year > 1 to exclude year record '0000' 

q2 <- dbGetQuery(conn,
  " SELECT planes.year AS manufacture_year, COUNT((ontime.DepDelay + ontime.ArrDelay) > 0) AS overallNum_delays
    FROM ontime JOIN planes ON ontime.TailNum = planes.tailnum
    WHERE ontime.Cancelled = 0 AND ontime.Diverted = 0 AND planes.year > 1 
    GROUP BY manufacture_year")

# remove last row with year record 'None'
q2 <- head(q2, -1)

# barplot 
barplot(height = q2$overallNum_delays, names = q2$manufacture_year, 
        col = brewer.pal(n = 6, name = "Set2"),
        space = 0.3,
        xlab = "Year of Manufacture of Planes",
        ylab = "Number of Overall Delay",
        main = "Total no. of departure and arrival delays by planes' year of manufacture in 2007 to 2008",
        ylim = c(0,800000))

# lowest: 349 counts of overall delay by planes manufactured in 1972
# highest: 774,994 counts by planes manufactured in 2001

# proportion by half of total year count
# 2008-1956 = 52, 52/2 = 26, 2008-26 = 1982
# sum counts for planes 26 years and younger (>= 1982) and planes older than 26 years (<1982)
q2_c1 <- with(q2, sum(overallNum_delays[manufacture_year < 1982]))
q2_c2 <- with(q2, sum(overallNum_delays[manufacture_year >= 1982]))

# percentage of counts for planes 26 years and younger & older than 26 years
q2_p1 <- round((q2_c1/sum(q2_c1 + q2_c2))*100, 2)
q2_p2 <- round((q2_c2/sum(q2_c1 + q2_c2))*100, 2)

# combine
q2_p <- data.frame(year = c("1956-1981", "1982-2008"),
                   count = c(q2_c1, q2_c2),
                   percentage = c(q2_p1, q2_p2))
View(q2_p)

# piechart
pie(q2_p$count, labels = q2_p$year, border = "white", col = brewer.pal(3, "Paired"))

# answer is no, older planes do not suffer more delays

# ==== Q3 ====
# How does the number of people flying between different locations change over time?

# no. of inbound flights for top 10 airports, cities, and states per month per year
# includes regular and diverted flights, excludes cancelled flights

# total inbound flights for airports, cities, states per month per year
q3 <- dbGetQuery(conn, 
  " SELECT 
    ontime.Year AS year, ontime.Month AS month, airports.airport AS airport, 
    airports.city AS city, airports.state AS state, COUNT(ontime.Dest) AS total_inbound
    FROM ontime JOIN airports ON airports.iata = ontime.Dest
    WHERE ontime.Cancelled = 0 
    GROUP BY airport, month
    ORDER BY year, month, total_inbound DESC")

# filter by every 10th row per month
# ie. show top 10 airports by total inbound flights per month per year
q3_new <- q3 %>%
  group_by(year, month) %>%
  arrange(year, month)  %>%
  mutate(rowNum = row_number(desc(total_inbound))) %>%
  filter(rowNum <= 10) 

# show seasonality using connected scatterplot (line + scatterplot)
ggplot(q3_new, aes(x = month, y = total_inbound)) +
  geom_line(linetype=2) +
  geom_point(color = "darkorange") +
  facet_grid(cols = vars(year)) +
  theme_light() +
  xlab("Month") +
  ylab("Total Inbound Flights") +
  ggtitle("Seasonality of Top 10 Total Inbound Flights in 2007 to 2008")


# ==== Q4 ====
# Can you detect cascading failures as delays in one airport create delays in others?

# interpret question as 'regardless of aircrafts departing on time/late from airport A/B, do they arrive late on schedule to airports B/C...'
# late aircraft delay refers to late arriving aircrafts
# from airport A to B via C -- A to C, C to B, or more stops
# query for same day, same flight aircrafts that departed late with LateAircraftDelay > 60 mins
q4 <- dbGetQuery(conn, 
  " SELECT ontime.Year AS year, ontime.Month AS month, ontime.DayofMonth AS date,
    ontime.FlightNum AS flight_num, ontime.TailNum AS aircraft, ontime.Origin AS origin, ontime.Dest AS destination,
    ontime.DepDelay AS dep_delay, ontime.LateAircraftDelay AS aircraft_delay
    FROM ontime JOIN airports ON ontime.Origin = airports.iata
    WHERE ontime.Cancelled = 0 AND ontime.Diverted = 0 AND dep_delay >= 0 AND aircraft_delay > 0
    ORDER BY year, month, date, flight_num, aircraft")

# filter to show same day, same flight, same aircraft
q4_new <- q4 %>%
  group_by(year, month, date, flight_num, aircraft) %>%
  filter(n()>1)

View(q4_new)

# first cascade failure as delay is on 1/1/2007, flight no. 12
# 'N563JB' departed from 'FLL' 55 mins late and arrived at 'JFK' 49 mins late
# and departed from 'JFK' 94 mins late and arrived at 'BTV' 78 mins late

# answer is yes, can detect cascading failures across one airport to others

# ==== Q5 ====
# Use the available variables to construct a model that predicts delays.

# response/dependent variable is delays (in mins) - continuous, use regression
# do linear, lasso, and ridge
# compare using benchmarking to choose better model

# create dataframe for modelling delays
# let 'delays' refer to arrival delay + departure delay, ie. overall delay (in mins)
# arrival delay consists of carrier + weather + NAS + security + late aircraft delays
q5 <- dbGetQuery(conn, 
  " SELECT DepTime AS dep_time, CRSDepTime AS sch_dep_time, ArrTime AS arr_time, CRSArrTime AS sch_arr_time,
    AirTime AS airtime, ArrDelay + DepDelay AS overall_delay, Distance AS distance, TaxiIn AS taxi_in, TaxiOut AS taxi_out    
    FROM ontime
    WHERE Cancelled = 0 AND Diverted = 0")

# load libraries for regression
library(mlr3)
library(mlr3pipelines)
library(mlr3learners)

# install.packages("skimr")
library(skimr)
# generate a report on the data to check for missing values
skim(q5)

# no missing values but too many rows
# create subset of 1000 rows without replacement at random
q5_sample <- sample_n(q5, 1000)

# set up task and response variable 'overall_delay'
task <- TaskRegr$new(id = 'q5', backend = q5_sample, target = 'overall_delay')
task$nrow

# set measure
measure <- msr('regr.mse')

# == linear regression ==
learner_lm <- lrn('regr.lm')

# impute mean of missing variables if any
gr_lm <- po('imputemean') %>>%
  po(learner_lm)

# combine learner
glrn_lm <- GraphLearner$new(gr_lm)

# set seed to reproduce results
set.seed(2022)

# train and test
# evalute performance by comparing predictions vs actual via mse
train_set <- sample(task$nrow, 0.7 * task$nrow)
test_set <- setdiff(seq_len(task$nrow), train_set)

glrn_lm$train(task, row_ids = train_set)
glrn_lm$predict(task, row_ids = test_set)$score()

# regr.mse 
# 3554.641  

# == ridge regression ==
# alpha = 0, lambda = 0.03 (temporary, find optimal later)
learner_ridge <- lrn('regr.glmnet') 
learner_ridge$param_set$values <- list(alpha = 0, lambda = 0.03)

gr_ridge <- po('scale') %>>%
  po('imputemean') %>>%
  po(learner_ridge)

glrn_ridge<- GraphLearner$new(gr_ridge)

glrn_ridge$train(task, row_ids = train_set)
glrn_ridge$predict(task, row_ids = test_set)$score() 

# regr.mse
# 3564.986  

# use tuning hyperparameters to find optimal lambda
# create second ridge learner
learner_ridge2 <- lrn('regr.glmnet') 
learner_ridge2$param_set$values <- list(alpha = 0)

gr_ridge2 <- po('scale') %>>%
  po('imputemean') %>>%
  po(learner_ridge2)

glrn_ridge2 <- GraphLearner$new(gr_ridge2)

# load libraries for tuning hyperparameter
library(mlr3tuning)
library(paradox)

# set up tuning environment
tune_lambda <- ParamSet$new (list(
  ParamDbl$new('regr.glmnet.lambda', lower = 0.001, upper = 2)))

tuner<-tnr('grid_search')
terminator <- trm('evals', n_evals = 20)

# put everything together in a new learner
at_ridge <- AutoTuner$new(
  learner = glrn_ridge2,
  resampling = rsmp('cv', folds = 3),
  measure = measure,
  search_space = tune_lambda,
  terminator = terminator,
  tuner = tuner)

# train the learner on the training data
at_ridge$train(task, row_ids = train_set)

# find optimal lambda
at_ridge$model
# optimal lambda is 2

# evaluate performance of optimal lambda
at_ridge$predict(task, row_ids = test_set)$score() 

# regr.mse 
# 3936.854  

# == lasso regression ==
# alpha = 1, optimal lambda = 2
learner_lasso <- lrn('regr.glmnet') 
learner_lasso$param_set$values <- list(alpha = 1, lambda = 2)

gr_lasso <- po('scale') %>>%
  po('imputemean') %>>%
  po(learner_lasso)

glrn_lasso<- GraphLearner$new(gr_lasso)

glrn_lasso$train(task, row_ids = train_set)
glrn_lasso$predict(task, row_ids = test_set)$score()

# regr.mse
# 4263.486  

# == benchmarking ==
set.seed(2022) 

# create list of learners
lrn_list <- list(
  glrn_lm,
  glrn_ridge,
  at_ridge,
  glrn_lasso)

# set the benchmark design and run the comparisons
bm_design <- benchmark_grid(task = task, resamplings = rsmp('cv', folds = 3), learners = lrn_list)
bmr <- benchmark(bm_design, store_models = TRUE)

# load libraries to plot
library(mlr3viz)

autoplot(bmr) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("MSE comparison of regression models")

bmr$aggregate(measure)