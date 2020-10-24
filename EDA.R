library(tidyverse)
seedval <- 1433
set.seed(seedval)

amesHousing<- read_csv(file = "./Data/train.csv")

#str(amesHousing)

numrows<-nrow(amesHousing)

# amesHousing %>% 
#   select_if(function(x) {any(is.na(x))}) %>% 
#   summarise(across(.cols = everything(),.fns=function(x){sum(is.na(x))})) %>% 
#   pivot_longer(cols = everything()) %>% 
#   mutate("% of Values Missing"= round((value/numrows)*100,digits = 2)) %>% 
#   select("Column Name" =name,"Number of Missing Values"=value,"% of Values Missing") 
# 
# 
# 
# View(amesHousing %>% 
#   summarise(across(.cols = function(x){any(is.na(x))},.fns=function(x){sum(is.na(x))})) %>% 
#   pivot_longer(cols = everything()) %>% 
#   mutate("% of Values Missing"= round((value/numrows)*100,digits = 2)) %>% 
#   select("Column Name" =name,"Number of Missing Values"=value,"% of Values Missing") %>% 
#   arrange(desc(`% of Values Missing`))
# )
# 
# 
# library(hash)
# # 
# #   20	1-STORY 1946 & NEWER ALL STYLES
# # 30	1-STORY 1945 & OLDER
# # 40	1-STORY W/FINISHED ATTIC ALL AGES
# # 45	1-1/2 STORY - UNFINISHED ALL AGES
# # 50	1-1/2 STORY FINISHED ALL AGES
# # 60	2-STORY 1946 & NEWER
# # 70	2-STORY 1945 & OLDER
# # 75	2-1/2 STORY ALL AGES
# # 80	SPLIT OR MULTI-LEVEL
# # 85	SPLIT FOYER
# # 90	DUPLEX - ALL STYLES AND AGES
# # 120	1-STORY PUD (Planned Unit Development) - 1946 & NEWER
# # 150	1-1/2 STORY PUD - ALL AGES
# # 160	2-STORY PUD - 1946 & NEWER
# # 180	PUD - MULTILEVEL - INCL SPLIT LEV/FOYER
# # 190	2 FAMILY CONVERSION - ALL STYLES AND AGES
# 
# hshMSSubClass<-hash(c("20",
#                       "30",
#                       "40",
#                       "45",
#                       "50",
#                       "60",
#                       "70",
#                       "75",
#                       "80",
#                       "85",
#                       "90",
#                       "120",
#                       "150",
#                       "160",
#                       "180",
#                       "190"
# ),
# c("1-STORY 1946 & NEWER ALL STYLES",
#   "1-STORY 1945 & OLDER",
#   "1-STORY W/FINISHED ATTIC ALL AGES",
#   "1-1/2 STORY - UNFINISHED ALL AGES",
#   "1-1/2 STORY FINISHED ALL AGES",
#   "2-STORY 1946 & NEWER",
#   "2-STORY 1945 & OLDER",
#   "2-1/2 STORY ALL AGES",
#   "SPLIT OR MULTI-LEVEL",
#   "SPLIT FOYER",
#   "DUPLEX - ALL STYLES AND AGES",
#   "1-STORY PUD (Planned Unit Development) - 1946 & NEWER",
#   "1-1/2 STORY PUD - ALL AGES",
#   "2-STORY PUD - 1946 & NEWER",
#   "PUD - MULTILEVEL - INCL SPLIT LEV/FOYER",
#   "2 FAMILY CONVERSION - ALL STYLES AND AGES"
# )
# )
# getValsFromDict <- function(dictvar,x){
# hash::values(dictvar,x)
# }
# 
# getValsFromDict(dictvar = hshMSSubClass,"120")
# 
# amesHousing %>% transmutate(MSSubClass1=hshMSSubClass$MSSubClass) %>% select(MSSubClass1)
# amesHousing %>% mutate(m1=getValsFromDict(dictvar = hshMSSubClass,x = MSSubClass)) %>% select(m1)

missing_values_replace <- list(PoolQC="NP",MiscFeature="NoMisc",Alley="NoAlley",Fence="NoFence",FireplaceQu="NoFirePl",
                               LotFrontage=0,GarageType="NoGarage",GarageFinish="NoGarage",GarageQual="NoGarage",GarageCond="NoGarage",
                               BsmtExposure="NB",BsmtFinType2="NB",BsmtQual="NB",BsmtCond="NB",BsmtFinType1="NB",MasVnrType="None",MasVnrArea=0,
                               Electrical="NoElec",GarageYrBlt=-1
                               )
amesHousing<-amesHousing %>% replace_na(replace = missing_values_replace )

amesHousing_mod %>% 
summarise(across(.cols = function(x){any(is.na(x))},.fns=function(x){sum(is.na(x))})) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate("% of Values Missing"= round((value/numrows)*100,digits = 2)) %>% 
  select("Column Name" =name,"Number of Missing Values"=value,"% of Values Missing") %>% 
  arrange(desc(`% of Values Missing`))





factor_cols<- c("MSSubClass",	"MSZoning",	"Street",	"Alley",	"LotShape",	"LandContour",	"Utilities",	"LotConfig",	"LandSlope",	"Neighborhood",	"Condition1",	"Condition2",	"BldgType",	"HouseStyle",	"OverallQual",	"OverallCond",	"RoofStyle",	"RoofMatl",	"Exterior1st",	"Exterior2nd",	"MasVnrType",	"ExterQual",	"ExterCond",	"Foundation",	"BsmtQual",	"BsmtCond",	"BsmtExposure",	"BsmtFinType1",	"BsmtFinType2",	"Heating",	"HeatingQC",	"CentralAir",	"Electrical",	"KitchenQual",	"Functional",	"FireplaceQu",	"GarageType",	"GarageFinish",	"GarageQual",	"GarageCond",	"PavedDrive",	"PoolQC",	"Fence",	"MiscFeature",	"SaleType",	"SaleCondition"
)

ordered_factor_cols<-c("LandSlope","OverallQual","OverallCond","ExterQual","ExterCond","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","HeatingQC","KitchenQual","Functional","FireplaceQu","GarageFinish","GarageQual","GarageCond")
unordered_factor_cols <- setdiff(factor_cols,ordered_factor_cols)

# amesHousing<-amesHousing %>% mutate(
#   across(.cols = all_of(unordered_factor_cols),.fns = function(x){as.factor(x)})
#   )

amesHousing<-amesHousing %>% mutate(
  across(.cols = all_of(unordered_factor_cols),.fns = ~factor(.x))
)




fLandSlope <- c("Gtl","Mod","Sev")
fOverallQual<- c(1:10)
fOverallCond<- c(1:10)
fExterQual<-c("Ex","Gd","TA","Fa","Po")
fExterCond<-c("Ex","Gd","TA","Fa","Po")
fBsmtQual<-c("Ex","Gd","TA","Fa","Po","NB")
fBsmtCond<-c("Ex","Gd","TA","Fa","Po","NB")
fBsmtExposure <- c("Gd","Av","Mn","No","NB")
fBsmtFinType1 <-c("GLQ","ALQ","BLQ","Rec","LwQ","Unf","NB")
fBsmtFinType2 <-c("GLQ","ALQ","BLQ","Rec","LwQ","Unf","NB")
fHeatingQC<-c("Ex","Gd","TA","Fa","Po")
fKitchenQual<-c("Ex","Gd","TA","Fa","Po")
fFunctional<-c("Typ","Min1","Min2","Mod","Maj1","Maj2","Sev","Sal")
fFireplaceQu<-c("Ex","Gd","TA","Fa","Po")
fGarageFinish<-c("Fin","RFn","Unf","NoGarage")
fGarageQual<-c("Ex","Gd","TA","Fa","Po","NoGarage")
fGarageCond<-c("Ex","Gd","TA","Fa","Po","NoGarage")  

#tvars<-c("LandSlope","OverallQual")

amesHousing<-amesHousing %>% mutate(
  across(.cols = all_of(ordered_factor_cols),.fns = ~factor(x=.x,levels = get(paste0("f",cur_column()))))
)
amesHousing %>% select(LandSlope)



View(amesHousing %>% group_by(YearBuilt) %>% summarise(NumHouses=n()))
amesHousing %>% summarise(max(YearBuilt),min(YearBuilt))





View(amesHousing)