library(tidyverse)
library(readxl)

#Importing the dataset
data <- read_excel("Data_Lv2_USDA_PackagedMeals.xlsx")

#Checking a random column
data[3733,]


#Factorizing the dataset om the serving size and data source
data1 <- data %>%
  mutate(serving_size_unit = as.factor(serving_size_unit),
         data_source = as.factor(data_source))

data1[2260,"household_serving_fulltext"]
data1[c(1470,1472,1514,1516),"household_serving_fulltext"]



#Changing serving slices from' 2.1/2\" ' SLICES to '2.05_Inch_Slices'
data1[2260,"household_serving_fulltext"] <- "2 0.5_Inch_Slices"
data1[1415,"household_serving_fulltext"] <- "2 0.5_Inch_Slices"
data1[1457,"household_serving_fulltext"] <- "2 0.5_Inch_Slices"


#Changing "1 \" SLICE"  to "1 Slices"
data1[c(1470,1472,1514,1516),"household_serving_fulltext"] <- "1 Slices"


#Changing 'PER 1 CUP" to "1 Cup"

data1 <- data1 %>%
  mutate(
    household_serving_fulltext=ifelse(household_serving_fulltext=="PER 1 CUP", "1 Cup", household_serving_fulltext)
  )




#Separating the 'household_serving_fulltext' to Quantity and Type
data2 <- data1 %>%
  separate(household_serving_fulltext, c("Quantity", "Type"), sep = " ")


#Converting it to title case
data2$Type=str_to_title(data2$Type)

data2$Type

#Changing more values in TYPE columns
data2 <- data2 %>%
  mutate(
    Type=ifelse(Type=="average"|Type=="Average","Average Strips", Type),
    Type=ifelse(Type=="pizza", "Pizza", Type),
    Type=ifelse(Type=="pattie"|Type=="Pattie", "Patty", Type),
    Type=ifelse(Type=="Entrée"|Type=="Entr?f©e|"|Type=="Entr??E", "Entree", Type),
    Type=ifelse(Type=="cup"|Type=="CUP", "Cup", Type),
)

#Separating the Quantity column
data2 <- data2 %>%
  separate(Quantity, c("Num", "Den"), sep = "/")

#Changing the Num and Den and changing the Quantity column for all non-null columns in 'Quantity'
data2 <- data2 %>%
  mutate(
    Num = as.numeric(Num),
    Den = as.numeric(Den),
    Quantity = ifelse(!is.na(Den), Num/Den, Num)
  )


data2$Quantity <- as.numeric(data2$Quantity)

summary(data2$Quantity)

data3 <- data2 %>%
  select(-c("Num", "Den","discontinued_date", "market_country")) 

#Changing typos and redundant row labels into one term
data3 <- data3 %>%
  mutate(
    Type=ifelse(Type=="Entr?f??E","Entree", Type),
    Type=ifelse(Type=="Links", "Link", Type),
    Type=ifelse(Type=="(Approx."|Type=="Slice", "Slices", Type),
    Type=ifelse(Type=="Sandwich,"|Type=="Sandwiches"|Type=="Sandwitch"|Type=="Sanwich"|Type=="Sandwish", "Slices", Type),
    Type=ifelse(Type=="Pkg."|Type=="Pkg"|Type=="Packages", "Package", Type),
    Type=ifelse(Type=="Pattie"|Type=="Pattie,", "Patty", Type),
    Type=ifelse(Type=="Pack."|Type=="Packs", "Pack", Type),
    Type=ifelse(Type=="Piece", "Pieces", Type),
    Type=ifelse(Type=="Biscuit"|Type=="Biscuit,", "Biscuits", Type),
    Type=ifelse(Type=="Frittatas", "Frittata", Type),
    Type=ifelse(Type=="Spring", "Spring Roll", Type),
    Type=ifelse(Type=="Egg", "Eggs", Type),
    Type=ifelse(Type=="Omelettes", "Omelet", Type),
    Type=ifelse(Type=="G"|Type=="GRM", "Grm", Type),
    Type=ifelse(Type=="Can", "Cans", Type),
    Type=ifelse(Type=="Burrito", "Burritos", Type),
    Type=ifelse(Type=="Wrap", "Wraps", Type),
    Type=ifelse(Type=="Taco", "Tacos", Type),
    Type=ifelse(Type=="Pocket", "Pockets", Type),
    Type=ifelse(Type=="HALF", "2 0.5_Inch_Slices", Type),
    Type=ifelse(Type=="1 STUFFED SANDWICH", "Sandwich", Type)
  )

#Replacing NAs according to the weight
data3 <- data3 %>%
  mutate(
    Type=ifelse(serving_size==56, "Oz", Type),
    Quantity=ifelse(serving_size==56, 2, Quantity),
    Type=ifelse(serving_size==70, "Cup", Type),
    Quantity=ifelse(serving_size==70, 2, Quantity),
    Type=ifelse(Type=="1/4", "Cup", Type),
    Quantity=ifelse(Type=="1/4", 0.25, Quantity),
    Type=ifelse(Type=="Of", "Package", Type),
    Quantity=ifelse(Type=="Of", 0.25, Quantity),
    Type=ifelse(Type=="Half", "0.5_inch_slices", Type),
    Quantity=ifelse(Type=="Half", 2, Quantity)
  )
#############################################################################
#INGREDIENT TEXT MANIPULATION

#Removing the "[[1]]" pattern in front of each ingredient row
data2$ingredients<- gsub("[[1]]","",data2$ingredients)


#Creating columns for healthy and unhealthy ingredients
data2$is_oats <- grepl("OATS", data2$ingredients, fixed = TRUE)
data2$is_carrots <- grepl("CARROTS", data2$ingredients, fixed = TRUE)
data2$is_quinoa <- grepl("QUINOA", data2$ingredients, fixed = TRUE)
data2$is_beans <- grepl("BEANS", data2$ingredients, fixed = TRUE)
data2$is_organic <- grepl("ORGANIC", data2$ingredients, fixed = TRUE)
data2$is_spinach <- grepl("SPINACH", data2$ingredients, fixed = TRUE)
data2$is_broccoli <- grepl("BROCCOLI", data2$ingredients, fixed = TRUE)
data2$is_chickpea <- grepl("CHICKPEA", data2$ingredients, fixed = TRUE)
data2$is_tomato <-grepl("TOMATO", data2$ingredients, fixed = TRUE)


data2$is_sugar <- grepl("SUGAR", data2$ingredients, fixed = TRUE)
data2$is_gluten <- grepl("GLUTEN", data2$ingredients, fixed = TRUE)
data2$is_cheese <- grepl("CHEESE", data2$ingredients, fixed = TRUE)
data2$is_corn_syrup <- grepl("CORN SYRUP", data2$ingredients, fixed = TRUE)
data2$is_salt <- grepl("SALT", data2$ingredients, fixed = TRUE)
data2$is_palm_oil <- grepl("PALM OIL", data2$ingredients, fixed = TRUE)
data2$is_shortening <- grepl("SHORTENING", data2$ingredients, fixed = TRUE)
data2$is_sdm_bnzt <- grepl("SODIUM BENZOATE", data2$ingredients, fixed = TRUE)
data2$is_msg <- grepl("MONOSODIUM GLUTAMATE", data2$ingredients, fixed = TRUE)
data2$is_processed <- grepl("PROCESSED", data2$ingredients, fixed = TRUE)
data2$is_cornstarch <- grepl("CORNSTARCH", data2$ingredients, fixed = TRUE)

 



#Converting TRUE/FALSE to 1/0
data2$is_oats <- ifelse(data2$is_oats == TRUE,1,0)
data2$is_carrots <- ifelse(data2$is_carrots == TRUE,1,0)
data2$is_quinoa <- ifelse(data2$is_quinoa == TRUE,1,0)
data2$is_beans <- ifelse(data2$is_beans == TRUE,1,0)
data2$is_organic <- ifelse(data2$is_organic == TRUE,1,0)
data2$is_spinach <- ifelse(data2$is_spinach == TRUE,1,0)
data2$is_broccoli <- ifelse(data2$is_broccoli == TRUE,1,0)
data2$is_chickpea <- ifelse(data2$is_chickpea == TRUE,1,0)
data2$is_tomato <- ifelse(data2$is_tomato == TRUE,1,0)

data2$is_sugar <- ifelse(data2$is_sugar == TRUE,1,0)
data2$is_gluten <- ifelse(data2$is_gluten == TRUE,1,0)
data2$is_cheese <- ifelse(data2$is_cheese == TRUE,1,0)
data2$is_corn_syrup <- ifelse(data2$is_corn_syrup == TRUE,1,0)
data2$is_salt <-ifelse(data2$is_salt == TRUE,1,0)
data2$is_palm_oil <-ifelse(data2$is_palm_oil == TRUE,1,0)
data2$is_shortening <-ifelse(data2$is_shortening == TRUE,1,0)
data2$is_sdm_bnzt <- ifelse(data2$is_sdm_bnzt == TRUE,1,0)
data2$is_msg <- ifelse(data2$is_msg == TRUE,1,0)
data2$is_processed <- ifelse(data2$is_processed == TRUE,1,0)
data2$is_cornstarch <- ifelse(data2$is_cornstarch == TRUE,1,0)

#Creating a target variable for unhealthy food which contains 3 or more classified unhealthy ingredients

#Creating a list for reference
unhealthy <- c(data2$is_sugar,data2$is_gluten,data2$is_cheese,data2$is_corn_syrup,data2$is_salt,data2$is_palm_oil,data2$is_shortening,data2$is_sdm_bnzt,data2$is_msg,data2$is_processed,data2$is_cornstarch )

data2$is_unhealthy <- ifelse (data2$is_sugar + data2$is_gluten + data2$is_cheese + data2$is_corn_syrup + data2$is_salt + data2$is_palm_oil +  data2$is_shortening + data2$is_sdm_bnzt + data2$is_msg + data2$is_processed + data2$is_cornstarch >= 3 ,1,0)


#Converting all other foods to healthy
data2$is_healthy <- ifelse (data2$is_unhealthy == 1,0,1)

#Checking the entries which have a value of 1 for is_healthy and is_unhealthy
ties <- data2$fdc_id[data2$is_healthy ==1 & data2$is_unhealthy ==1]
ties
data2 <- data2[!(data2$fdc_id %in% ties),]

#Checking the difference in length to check if i have missed any values
length(data2$fdc_id)  - length(data2$fdc_id) 

#Comparing that with the ties variable: Everything looks fine
length(ties)

#############################################################################






data3$Type <- as.factor(data3$Type)
summary(data3$Type)
nlevels(data3$Type)

#Factorizing brand_owner
data2$brand_owner <- as.factor(data2$brand_owner)
data2$branded_food_category <- as.factor(data2$branded_food_category)
summary(data2$branded_food_category)
nlevels(data2$branded_food_category)

##############################################################################

#LOGISTIC REGRESSION


complete_test_mark <- sample(nrow(data2), 0.2*nrow(data2))
final_test_set = data2[complete_test_mark,]
final_train_set = data2[-complete_test_mark,]
df_2 = data2[-complete_test_mark,]


#Splitting df2 into train and  test split

train_mark <- sample(nrow(df_2), 0.8*nrow(df_2))
train_data <- df_2[train_mark,]
test_data <- df_2[-train_mark,]



logistic_model_1 <- glm(formula =  is_unhealthy ~   branded_food_category  , data = train_data, family = 'binomial')
summary(logistic_model_1)
predictions_1 <- predict(logistic_model_1, newdata = test_data, type = 'response')
classifications_1 <- ifelse( predictions_1 > 0.5 ,1,0)

results_1 <- ifelse(classifications_1 == test_data$is_unhealthy,1,0)

sum(results_1)/length(results_1)


train_log_model <- function(formula) #Function for training model
{
  logistic_model <- glm(formula =  formula  , data = train_data, family = 'binomial')
  summary(logistic_model)
  return(logistic_model)
}


predict_log_model <- function(model,cutoff) #Formula for getting predictions
{
  predictions <- predict(model, newdata = test_data, type = 'response')
  classifications <- ifelse( predictions > cutoff ,1,0)
  results <- ifelse(classifications == test_data$is_unhealthy,1,0)
  CM <- table(test_data$is_unhealthy,classifications)
  
  print(CM)
  
  TP <- CM[2,2]
  TN <- CM[1,1]
  FP <- CM[1,2]
  FN <- CM[2,1]
  
  TPR <- TP / (TP+FN) 
  TNR <- TN / (TN+FP)
  FPR <- FP / (TN+FP)
  FNR <- FN / (TP+FN)
  
  print("The accuracy is:")
  print(sum(results)/length(results))
  
  
  print("The TPR is:")
  print(TPR)
  
  print("The TNR is:")
  print(TNR)
  
  print("The FNR is:")
  print(FNR)
  
  print("The FPR is:")
  print(FPR)
  
}






#Formula 1
fmla_1 = is_unhealthy ~   branded_food_category 

log_model_1 <- train_log_model(fmla_1)
summary(log_model_1)



predict_log_model(log_model_1, 0.5) #0.6344


predict_log_model(log_model_1, 0.45) #0.7901  

#Trying for different values of cutoff
predict_log_model(log_model_1, 0.25) #0.71
predict_log_model(log_model_1, 0.40) #0.7901
###################################################################
predict_log_model(log_model_1, 0.35) #0.7927 --The best accuracy 
###################################################################
predict_log_model(log_model_1, 0.30) #0.719



sort(unique(train_data$brand_owner))


#Formula 2

fmla_2  = is_unhealthy ~ brand_owner
log_model_2 <- train_log_model(fmla_2)
summary(log_model_2)
#Trying for different values of cutoff

factor(test_data$brand_owner)
#Since there could be options where the brand might not appear in the test_data, the following line makes sure they come up in the model$xlevels
log_model_2$xlevels[["brand_owner"]] <- union(log_model_2$xlevels[["brand_owner"]], levels(test_data[["brand_owner"]]))

predict_log_model(log_model_2, 0.5)

#Accuracy shows up as NA, let's try a different formula:
log_model_2.1_accuracy <- (317+231)/(317+84+72+231)
###################################################################
print(log_model_2.1_accuracy)  #0.77 --the best accuracy
###################################################################
#Trying a different cutoff value
predict_log_model(log_model_2, 0.35)
log_model_2.2_accuracy <- (263 + 236) /(263+126+54+261) 
print(log_model_2.2_accuracy) #.70

predict_log_model(log_model_2, 0.75)
log_model_2.3_accuracy <- (353 + 167) /(353+167+148+36) 
print(log_model_2.3_accuracy) #.73

unique(train_data$serving_size_unit)

#Adding an additional column that shows if a food is a liquid or a solid based on the serving size

data2$serving_size_unit[is.na(data$serving_size_unit) == TRUE] <- 'g'
data2 <- data2 %>%
  mutate(is_liquid = ifelse(serving_size_unit == 'g',0,1))
  

train_data$serving_size_unit[is.na(train_data$serving_size_unit) == TRUE] <- 'g'
train_data <- train_data %>%
  mutate(is_liquid = ifelse(serving_size_unit == 'g',0,1))


test_data$serving_size_unit[is.na(test_data$serving_size_unit) == TRUE] <- 'g'
test_data <- test_data %>%
  mutate(is_liquid = ifelse(serving_size_unit == 'g',0,1))

final_train_set$serving_size_unit[is.na(final_train_set$serving_size_unit) == TRUE] <- 'g'
final_train_set <- final_train_set %>%
  mutate(is_liquid = ifelse(serving_size_unit == 'g',0,1))

final_test_set$serving_size_unit[is.na(final_test_set$serving_size_unit) == TRUE] <- 'g'
final_test_set <- final_test_set %>%
  mutate(is_liquid = ifelse(serving_size_unit == 'g',0,1))



#Formula 3
fmla_3 <- is_unhealthy~is_liquid + branded_food_category 
log_model_3 <- train_log_model(fmla_3)
summary(log_model_3)
predict_log_model(log_model_3, 0.5)


summary(log_model_1)
summary(log_model_2)
summary(log_model_3)




#######################
#USING THE FINAL DATASET



train_final_model <- function(formula) #Function for training model
{
  logistic_model <- glm(formula =  formula  , data = final_train_set, family = 'binomial')
  summary(logistic_model)
  return(logistic_model)
}


predict_final_model <- function(model,cutoff) #Formula for getting predictions
{
  predictions <- predict(model, newdata = final_test_set, type = 'response')
  classifications <- ifelse( predictions > cutoff ,1,0)
  results <- ifelse(classifications == final_test_set$is_unhealthy,1,0)
  CM <- table(final_test_set$is_unhealthy,classifications)
  
  print(CM)
  
  TP <- CM[2,2]
  TN <- CM[1,1]
  FP <- CM[1,2]
  FN <- CM[2,1]
  
  TPR <- TP / (TP+FN) 
  TNR <- TN / (TN+FP)
  FPR <- FP / (TN+FP)
  FNR <- FN / (TP+FN)
  
  print("The accuracy is:")
  print(sum(results)/length(results))
  
  
  print("The TPR is:")
  print(TPR)
  
  print("The TNR is:")
  print(TNR)
  
  print("The FNR is:")
  print(FNR)
  
  print("The FPR is:")
  print(FPR)
  
}

final_model_3 <- train_final_model(fmla_3)
summary(final_model_3)


final_model_3$xlevels[["branded_food_category"]] <- union(final_model_3$xlevels[["branded_food_category"]], levels(final_test_set[["branded_food_category"]]))
predict_final_model(final_model_3, 0.5) #Our model is 80.6% Accurate with low false positives
predict_final_model(final_model_3, 0.46) # 80.6%
predict_final_model(final_model_3, 0.75) # 77.3%
predict_final_model(final_model_3, 0.35) # 80%
###########################################################################
#INFERENCE




## 1: "Breakfast Sandwiches, Biscuits & Meals"is the food type that has the greatest number of predicted unhealthy food
## 2: "Ready-Made Combination Meals " , "Vegetable Based Products / Meals" are the meals that are considered the least unhealthy food types 
## 3: I can't find a brand that can be classified as just making unhealthy food in this dataset.
## 4: Our model showed an accuracy of 80.6% with low false positives. However the situation would have been more ideal if there were lower false negatives as they could cause more harm in the long run.


write_csv(data2,"final_dataset.csv")

