clear 
clear matrix
clear mata
set more off
set mem 300m
set maxvar 9000

/*
This is Stata do file to reproduce analysis and results for a published paper: 
"Reporting sterilization as a current contraceptive method among sterilized women: 
lessons learned from a population with high sterilization rates, Rajasthan, India" 
(Contraception. 2019 Feb;99(2):131-136). The abstract and link to the full paper is 
[available here](https://www.ncbi.nlm.nih.gov/pubmed/30391289).  

There are three sections in this document for: accessing public data; 
preparing the analysis data file (i.e., data processing/manipulation); and 
conducting analysis. Analysis section contains code only for reproducing results 
presented in the paper. 

The paper was prepared using Stata and equivalent R Markdown fileand HTML output 
is available at [GitHub](https://github.com/yoonjoung?tab=projects)
*/ 

*********************************************************
***** 1. Data access
*********************************************************

/*
Data came from the Performance Monitoring and Accountability 2020 (PMA2020) survey 
conducted in Rajasthan, India, in early 2017. PMA2020 survey data are available for 
the public, and can be accessed here: https://www.pma2020.org/request-access-to-datasets. 
Request and download “Round 2 (2017) Rajasthan Household/Female survey” data. 
The file has been saved as “INR2_Rajasthan_HHQFQ.dta” in Stata format in a directory.
*/

*********************************************************
***** 2. Data management 
*********************************************************	

/*
(If you run this markdown file on your computer, you will need to change the file path 
according to your directory setting).
*/
cd "C:\YJ\Data PMA\rawHHQFQ"
use INR2_Rajasthan_HHQFQ.dta, clear

*2.1. Keep only relevant analysis sample 

/*
The unit of analysis is a female respondent in the survey. Since the public datafile include 
both household and female level observations, keep only female level observations. 
Then, keep only completed interviews with de facto sample. The analysis dataset now has 6015 
observations, all de-facto women 15-49 years of age who live in the sampled households.

NOTE: In the below code chunk, “keep if usually_live==1” should have been “keep if last_night==1 ”:
i.e., there was an error in classifying de-factor vs. de-jure popualtion, though small 
(2.1% of women who completed interview, 129/6089). The correct total number of de-factor 
population is 6034. Revised analysis results (not presented here, but can be produced with 
the changed code, “dta<-filter(dta, last_night==1)”) are very comparable with published results.
In any case, YC is responsible for this error. 
*/

keep if FQmeta~=""
keep if HHQ_result==1 & FRS_result==1 	
keep if usually_live==1
*keep if last_night==1 
	
*2.2. Create analysis variables 

	gen xweight=FQweight/1000000
	
	***** BASIC BACKGROUND	
	
	gen xage=FQ_age
	egen xagegroup5 = cut(FQ_age), at(15,20,25,30,35,40,45,50)
	lab var xage "woman's age at interview" 
	lab var xagegroup5 "woman's age at interview, 5-year group" 	
	tab xagegroup5, gen(xagegroup5_)
		
	gen xunion		=(FQmarital_status==1 | FQmarital_status==2)
	gen xmarried	=(FQmarital_status==1)
	
	gen xsa=  ///
		( (last_time_sex==1 & last_time_sex_value<=30 & last_time_sex_value>=0) ///
		| (last_time_sex==2 & last_time_sex_value<=4 & last_time_sex_value>=0)  ///
		| (last_time_sex==3 & last_time_sex_value<=1 & last_time_sex_value>=0) )
			
	gen xurban		=ur==1
		
	gen xwealth5	= wealthquintile
	gen xpoor		= wealthquintile==1
	gen xrich		= wealthquintile==5	
	tab xwealth5, gen(xwealth5_)
	
	gen xedu3		=0
		replace xedu3=1 if school>=1
		replace xedu3=2 if school>=3 
	tab xedu3, gen(xedu3_)	
	gen xedupri	=xedu3>=1
	gen xedusec	=xedu3>=2
	foreach x of varlist xedu*{
		replace `x'=. if school<0 | school==.
		}
		
	gen xcaste_sc 	= caste==1
	gen xcaste_st 	= caste==2
	gen xcaste_obc	= caste==3
	gen xcaste4=caste
	foreach x of varlist xcaste*{
		replace `x'=. if caste<0 | caste==.
		}
		
	gen xhindu 		= religion==1	
	gen xreligion 	= .
		replace xreligion=1 if religion==1
		replace xreligion=2 if religion==2
		replace xreligion=3 if religion>=3 		
	
	***** Sterilization

	gen byte xst_report	= femalester==1  	
	gen byte xst_probe	= sterilization_probe ==1
	gen byte xsteril	= xst_report==1 | xst_probe==1 
	
	lab var xst_report 	"reported F sterilization as a current method"
	lab var xst_probe 	"reported ever been sterilized" /*only asked to all who didn't report sterilization as a current method*/
	lab var xsteril 	"currently using F sterilization OR ever been sterilized"	
	
	tab xst_*, m
	
	***** Current method use 
	gen xuse=0
		replace xuse=1 if current_user==1
		replace xuse=2 if current_methodnum>=2 & current_methodnum<30 
		replace xuse=3 if current_methodnum==1

	lab var xuse "4 category current use reporting"
	lab define xuse 0 "none" 1 "traditional" 2 "modern" 3 "sterilization" 		
	lab values xuse xuse
	
	***** Time since sterilization 
	gen double xinterview	=FQdoi_correctedSIF	
	gen double xbegin		=begin_usingSIF
	
	foreach var of varlist xinterview xbegin{
		gen temp = dofc(`var')
		format %td temp
		gen `var'_mo = month(temp)
		gen `var'_yr = year(temp)
		drop temp		
		
		gen `var'_cmc = 12*(`var'_yr - 1900) + `var'_mo
		}		
	gen xtime= 	xinterview_cmc - xbegin_cmc
		replace xtime=. if xsteril==0
		lab var xtime "months since sterilization"
		tab xtime, m		
	gen xtime_yr = int(xtime/12)
	egen xtime_yr5 = cut(xtime_yr), at(0, 5, 10, 15, 20, 30) 
	tab xtime_yr5, gen(xtime_yr5_)
	
	lab var xinterview_cmc 	"timing of interview (CMC)"	
	lab var xbegin_cmc 	"timing of sterilization (CMC)" 	
	lab var xtime_yr 	"number of years since sterilization" 
	lab var xtime_yr5 	"number of years since sterilization (5-year group)" 	
		
*********************************************************
***** Analysis 
*********************************************************
/*
There are three tables in results. All estimates (e.g., proportion, odds ratio) were 
adjusted for survey sample design, since PMA2020 household surveys are sample surveys \
(see details regarding the sample design here and here). The number of observations 
in all tables is unweighted number.
*/

svyset [pweight=xweight], strata(ur) psu(EA_ID) singleunit(centered)

***** Among all women in the analysis sample 
codebook FQmeta /*n=6015 women*/	

	***** sterilization level (reported OR probed)
	svy: tab xsteril,  ci format(%7.3f)
	tab xsteril, m

	***** Checking unweighted numbers 
	foreach x of varlist xagegroup5 xmarried xsa xurban xwealth5 xedu3 xcaste4 xreligion xst_report xst_probe xsteril xtime_yr5 {
		tab `x' 
		}
	foreach x of varlist xagegroup5 xmarried xsa xurban xwealth5 xedu3 xcaste4 xreligion  {
		tab `x' xsteril
		}
	foreach x of varlist xagegroup5 xmarried xsa xurban xwealth5 xedu3 xcaste4 xreligion  {
		tab `x' xsteril if xweight~=.
		}
				
	***** TABLE 1. sample characteristics
	svy: mean xage 			
	svy: mean xage if xsteril==0	
	svy: mean xage if xsteril==1

	foreach x of varlist xagegroup5 xmarried xsa xurban xwealth5 xedu3 xcaste4 xreligion {
		svy: tab `x', format(%7.3f)
		}			
	foreach x of varlist xagegroup5 xmarried xsa xurban xwealth5 xedu3 xcaste4 xreligion {
		svy: tab `x' xsteril, col format(%7.3f)
		}	

	tab xtime_yr5 xsteril, m	
	svy: tab xtime_yr5 xsteril, col format(%7.3f)
	
***** Among those EVER sterilized, reporting current contraceptive method use 
keep if xsteril==1
codebook FQmeta /*n=1840 ALL sterilized women*/
	
	svy: tab xuse, col ci format(%7.3f)
	svy: tab xst_report, col ci format(%7.3f)

	***** TABLE 2
	foreach x of varlist xagegroup5 xmarried xsa xurban xwealth5 xedu3 xcaste4 xreligion xtime_yr5{
		svy: tab `x' xst_report, row format(%7.3f)
		}		
		
	***** TABNLE 3: bivariate	
	svy: logistic xst_report xagegroup5_3 - xagegroup5_7	
	svy: logistic xst_report xmarried 
	svy: logistic xst_report xsa 	
	svy: logistic xst_report xurban 
	svy: logistic xst_report xedupri	
	svy: logistic xst_report xpoor xrich 	
	svy: logistic xst_report xcaste_*	
	svy: logistic xst_report xhindu			
	svy: logistic xst_report xtime_yr5_2 - xtime_yr5_5 
	
	***** TABNLE 3: multivariable 
	svy: logistic xst_report xagegroup5_3 - xagegroup5_7 xmarried xsa ///
	xurban xedupri xpoor xrich xcaste_* xhindu xtime_yr5_2 - xtime_yr5_5  
	
*END OF THIS FILE 
