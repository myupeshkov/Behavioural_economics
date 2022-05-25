
#####import libraries

library("memisc")
library("dplyr")
library("psych")
library("foreign")
library("hexbin")
library("car")
library("sandwich")
library("tidyverse") 
library('estimatr')

library("foreign")
library("pander")

library("shiny")
library("rmarkdown")
library("texreg")

library("stargazer")

library("glmx")   
library("plm")
library("xtable")
library("lmtest")

library("pglm")                                          

#####download data

df <- read.csv('final_data.csv') #for each participant in each round

df$gender[(df$gender == "Male")] <- 0
df$gender[(df$gender == "Female")] <- 1

df$sex <- strtoi(df$gender)


data <- read.csv('data.csv') #for each participant average


#description table about participants
stargazer(data)



##### OLS regressions (average values)

#subject risk
model.baseline10 <- lm(mean_side~age+sex+gpa+inc+
             +holt_risk,
             data)

model.baseline11 <- lm(mean_side~age+sex+gpa+inc+
                         +mean_cat_person+holt_risk,
                       data=data)


model.baseline12 <- lm(mean_side~age+sex+gpa+inc+
                        +subject_risk,
                      data)

model.baseline13 <- lm(mean_side~age+sex+gpa+inc+
                          +mean_cat_person+subject_risk,
                      data)

#holt risk

model.baseline20 <- lm(cheat_activity~age+sex+gpa+inc+
                          +holt_risk,
                        data)

model.baseline21 <- lm(cheat_activity~age+sex+gpa+inc+
                          +mean_cat_person+holt_risk,
                        data=data)


model.baseline22 <- lm(cheat_activity~age+sex+gpa+inc+
                          +subject_risk,
                        data)

model.baseline23 <- lm(cheat_activity~age+sex+gpa+inc+
                          +mean_cat_person+subject_risk,
                        data)


#summary table with glm regressions
stargazer(model.baseline10, model.baseline11, model.baseline12,model.baseline13,
          model.baseline20, model.baseline21, model.baseline22,model.baseline23)



#####   PANEL REGRESSIONS 

#subject risk
model10 <- plm(flg_max_num~age+sex+gpa+inc
               +categ_num+categ_person+
                 +subject_risk,
               df,index=c("id","round"), model='random')

model11 <- plm(flg_max_num~age+sex+gpa+inc
               +categ_person+
                 +subject_risk,
               df,index=c("id","round"), model='random')

model12 <- plm(flg_max_num~age+sex+gpa+inc
               +categ_num+
                 +subject_risk,
               df,index=c("id","round"), model='random')

#holt risk
model20 <- plm(flg_max_num~age+sex+gpa+inc
              +categ_num+categ_person+
                +holt_risk,
              df,index=c("id","round"), model='random')


model21 <- plm(flg_max_num~age+sex+gpa+inc
              +categ_person+
                +holt_risk,
              df,index=c("id","round"), model='random')

model22 <- plm(flg_max_num~age+sex+gpa+inc
              +categ_num+
                +holt_risk,
              df,index=c("id","round"), model='random')

#without risk

model3 <- plm(flg_max_num~age+sex+gpa+inc
              +categ_num+categ_person,
              df,index=c("id","round"), model='random')

#summary table with panel regressions
stargazer(model10, model11, model12,
          model20, model21, model22, model3)


