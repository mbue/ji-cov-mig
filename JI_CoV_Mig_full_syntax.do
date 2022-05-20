**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************

*** Author of the Syntax: 
*** Marvin Bürmann
*
*** Project: 
*** Research Article "Did Immigrants Perceive More Job Insecurity during the SARS-CoV-2 Pandemic? Evidence from German Panel Data"
*
*** Authors of the Article: 
*** Marvin Bürmann, Jannes Jacobsen, Cornelia Kristen, Simon Kühne and Dorian Tsolak

**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
* SET-UP

version 17.0
clear all
capture log close
set more off
set matsize 800
set scheme s1mono

set emptycells keep, permanently

* GLOBAL PATHS
global MASTER "[MASTER-PATH]"
global SOEPV36 "[SOEPV36-PATH]"
global SOEPCOV "[SOEPCOV-PATH]"
capture mkdir "$MASTER\derived_data"
global DERIVED "$MASTER\derived_data"
capture mkdir "$MASTER\logfiles"
global LOGFILES "$MASTER\logfiles"
capture mkdir "$MASTER\results"
global RESULTS "$MASTER\results"

* LOGFILE
log using "$LOGFILES\ji_cov_mig_dataprep_smcl.smcl", replace

display "$S_TIME  $S_DATE"

**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
* MERGING

* SOEP-Cov Data (Weights 2020)
use "$SOEPCOV\2020-07-22_SOEP_CoV_Gewichte.dta", clear
// note:
// provided by the SOEP-CoV team before the v37 release
// not part of the regular SOEP v36 release
// variable names in the v37 release may differ

gen syear=2020
save "$DERIVED\2020-07-22_SOEP_CoV_Gewichte_syear.dta", replace

use "$SOEPV36\raw\phrf.dta"
keep pid bjphrf
gen syear=2019
save "$DERIVED\bjphrf.dta", replace

* SOEP-Cov Data (Individual Characteristics)
use "$SOEPCOV\soep_cov_20210421.dta", clear
// note: 
// provided by the SOEP-CoV team before the v37 release
// not part of the regular SOEP v36 release
// variable names in the v37 release may differ

merge 1:1 pid syear using "$DERIVED\bjphrf.dta", gen(merge_bjphrf)
replace phrf = bjphrf if syear==2019
drop if merge_bjphrf==2

merge 1:1 pid syear using "$DERIVED\2020-07-22_SOEP_CoV_Gewichte_syear.dta", gen(merge_cov_weights)
xtset pid syear

* SOEP-Cov Data (Information for Weights in 2021)
merge 1:1 pid syear using "$SOEPCOV\soep_cov_nonresponse_20_21.dta", gen(merge_weight21)
// note: 
// provided by the SOEP-CoV team before the v37 release
// not part of the regular SOEP v36 release
// variable names in the v37 release may differ

merge 1:1 pid syear using "$SOEPV36\raw\bjp.dta", keepusing(pid syear bjp_53 bjp_54 bjp_49_nace2) gen(merge_bjp)
merge 1:1 pid syear using "$SOEPV36\pgen.dta", gen(merge_pgen_2019)
merge 1:1 pid syear using "$SOEPV36\raw\bjpgen.dta", keepusing(pid syear emplst19 stib19 allbet19 ausb19 expue19 oeffd19 erljob19 siops08_19 partz19 bjfamstd bjerwzeit) gen(merge_bjpgen)
merge m:1 hid syear using "$SOEPV36\raw\bjh.dta", keepusing(hid syear bjh_61_01) gen(merge_bjh)
merge 1:1 pid syear using "$SOEPV36\pl.dta", keepusing(pid syear plh0032 plh0033 plj0071 plj0072 plj0073 plj0698 plj0699 plj0700 plj0722 plj0680_v2 plj0680_v1 pli0046 plh0258_h) gen(merge_pl2)
merge m:1 hid syear using "$SOEPV36\hgen.dta", keepusing(hid syear hghinc) gen(merg_hgen)

keep hid syear pid psample pgkldb2010 pcovwkt3 pgisced11 pgstib pgallbet pgausb pgoeffd pgerljob pglabnet pglabgro stib19 allbet19 emplst19 ausb19 expue19 oeffd19 erljob19 partz19 plb0022_h pgemplst pcovjob1 pgsiops08 emplst19 siops08_19 pgvebzeit pgnace2 ple0010_h gebjahr sex pgfamstd bjfamstd pgpartz hlk0044 pgerwzeit bjerwzeit plh0171 pgexpue migback plh0258_h hghinc bjh_61_01 plh0032 plh0033 plb0568 plj0071 plj0072 plj0073 plj0698 plj0699 plj0700 pgpbbila pgnation plj0722 plj0680_v2 plj0680_v1 immiyear pli0046 phrf phrf_cati nrweight merge_pgen merge_pl merge_cov_weights merge_h2019 merge_bjp bjp_53 bjp_54 bjp_49_nace2

merge 1:1 pid syear using "$SOEPV36\raw\bep.dta", keepusing(pid syear bep37 bep38) gen(merge_bep)
merge 1:1 pid syear using "$SOEPV36\raw\bfp.dta", keepusing(pid syear bfp60 bfp61) gen(merge_bfp)
merge 1:1 pid syear using "$SOEPV36\raw\bgp.dta", keepusing(pid syear bgp56 bgp57) gen(merge_bgp)
merge 1:1 pid syear using "$SOEPV36\raw\bhp.dta", keepusing(pid syear bhp_59 bhp_60) gen(merge_bhp)
merge 1:1 pid syear using "$SOEPV36\raw\bip.dta", keepusing(pid syear bip_69 bip_70) gen(merge_bip)

merge 1:1 pid syear using "$SOEPV36\pequiv.dta", keepusing(pid syear h11101 h11102 h11103 h11104 h11105 h11106 h11107 h11108 h11109) gen(merge_pequiv2)

drop migback sex immiyear
merge m:1 pid using "$SOEPV36\ppath.dta", keepusing(pid migback sex immiyear immiyearinfo) gen(merge_ppath)

keep if syear>=2014 & syear!=.

keep if merge_pgen==3 | merge_pl==3 | merge_h2019==3 | merge_cov_weights==3 | syear==2021

save "$DERIVED\soepv36_soepcov_merged.dta", replace

**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
* VARIABLES

use "$DERIVED\soepv36_soepcov_merged.dta", clear

* weights
xtset pid syear
gen phrf_new=phrf
replace phrf_new = phrf_cati if syear==2020
replace phrf_new = l.phrf_cati*nrweight if syear==2021

* fear of job loss
xtset pid syear
tab pcovwkt3
recode pcovwkt3 (-2=.), gen(fear_jobloss)
gen fear_jobloss_f = f.fear_jobloss
gen fear_jobloss_l = l.fear_jobloss

* financial worries
recode plh0033 (-5 -2 -1=.), gen(sorgen_e_wirtsch_raw)
gen sorgen_e_wirtsch = 3 - sorgen_e_wirtsch_raw
lab def sorgen_e_wirtsch 0 "no worries" 1 "some worries" 2 "many worries"
lab val sorgen_e_wirtsch sorgen_e_wirtsch

* generation status
recode migback (-2=.) (1=0) (2=1) (3=.), gen(dir_migback)
lab def dir_migback  0 "majority" 1 "1st gen immig."
lab val dir_migback dir_migback

recode migback (-2=.) (2/3=.), gen(migback_full)
replace migback_full = 2 if migback==3
replace migback_full = 3 if migback==2
lab def migback_full 1 "majority" 2 "2nd gen immig." 3 "1st gen immig."
lab val migback_full migback_full

* years since migration
gen ysm = syear - immiyear if immiyear>=0 & immiyear!=.

* female
recode sex (-3 -1=.)
recode sex (1=0) (2=1), gen(female)
lab def female 0 "male" 1 "female"
lab val female female

* age in years
recode ple0010_h (-1=.), gen(geburtsjahr)
replace geburtsjahr = gebjahr if geburtsjahr==. & syear==2014
gen alter = syear - geburtsjahr

* educational attainment
recode pgisced11 (-1 -8=.) (0/2=1) (3/5=2) (6/8=3), gen(qualifikation_isced)
lab def qualifikation_isced 1 "low" 2 "medium" 3 "high"
lab val qualifikation_isced qualifikation_isced

* (highest) degree acquired abroad
mvdecode pgisced11, mv(-2 = .a \ -1 = .b)
mvdecode pgpbbila, mv(-2 = .a \ -1 = .b \ -5 = .c)

gen isced11_ausl = .
replace isced11_ausl = 1 if pgpbbila == 1 | pgpbbila == 11 
replace isced11_ausl = 3 if pgpbbila == 2 | pgpbbila == 3 | pgpbbila == 12 | pgpbbila == 13 | pgpbbila == 16 
replace isced11_ausl = 6 if pgpbbila == 4 | pgpbbila == 14 | pgpbbila == 17 
replace isced11_ausl = 8 if pgpbbila == 9 | pgpbbila == 19 

gen diff_pgisced11_isced11_ausl = pgisced11 - isced11_ausl if pgisced11!=. & isced11_ausl!=.

gen abschl_ausl = 1 if diff_pgisced11_isced11_ausl == 0
replace abschl_ausl = 0 if diff_pgisced11_isced11_ausl > 0 & diff_pgisced11_isced11_ausl!=.
replace abschl_ausl = . if migback!=2
replace abschl_ausl = 0 if pgisced11!=. & abschl_ausl==. & migback==2
lab def abschl_ausl 0 "highest degree GER" 1 "highest degree abroad"
lab val abschl_ausl abschl_ausl

* German language proficiency
gen deutsch_sprechen = 5 - plj0071 if plj0071>0
gen deutsch_schreiben = 5 - plj0072 if plj0072>0
gen deutsch_lesen = 5 - plj0073 if plj0073>0
egen deutsch_gesamt = rowmean(deutsch_lesen deutsch_schreiben deutsch_sprechen)

rename deutsch_gesamt deutsch_gesamt_once

tsset pid syear
bys pid: carryforward deutsch_gesamt_once, gen(deutsch_gesamt) 

bys pid: egen deutsch_gesamt_mean = mean(deutsch_gesamt)
replace deutsch_gesamt = deutsch_gesamt_mean if deutsch_gesamt==.

* self-assessed health
recode plh0171 (-5 -1=.), gen(subjective_health)
lab val subjective_health plh0171

* Occupational status (SIOPS)
foreach var in siops08 {
gen `var' = pg`var' if pg`var'>=0
replace `var' = `var'_19 if syear==2019 & `var'_19>=0
lab val `var' pg`var'
}

* employment status (incl. part-time work (vs. full-time work))
recode plb0022_h (-5 -1=.) (5/7=.) (10/12=.) (8=6) (9=5), gen(emplst)
lab val emplst pgemplst

* short-time work
recode pcovjob1 (-2=.), gen(kurzarbeit)
replace kurzarbeit = 0 if emplst==1 | emplst==2

* employment status in 2019
gen emplst2019_help = emplst19 if syear==2019
bys pid: egen emplst2019 = min(emplst2019_help)
drop emplst2019_help
replace emplst = emplst2019 if kurzarbeit==1

* employment status incl. short-time
recode emplst (1 2=1) (3=.) (4=2) (5=4), gen(emplst_short)
replace emplst_short = 3 if kurzarbeit==1
lab var emplst "employment status"
lab def emplst_short 1 "FT/PT" 2 "marginal empl." 3 "short-time" 4 "unemployed"
lab val emplst_short emplst_short

* subcontracted work
recode bep37 (-2 -1=.) (2=0), gen(zeitarbeit)
replace zeitarbeit = 1 if bfp60==1 & syear==2015
replace zeitarbeit = 0 if bfp60==2 & syear==2015
replace zeitarbeit = 1 if bgp56==1 & syear==2016
replace zeitarbeit = 0 if bgp56==2 & syear==2016
replace zeitarbeit = 1 if bhp_59==1 & syear==2017
replace zeitarbeit = 0 if bhp_59==2 & syear==2017
replace zeitarbeit = 1 if bip_69==1 & syear==2018
replace zeitarbeit = 0 if bip_69==2 & syear==2018
replace zeitarbeit = 1 if bjp_53==1 & syear==2019
replace zeitarbeit = 0 if bjp_53==2 & syear==2019
xtset pid syear

* fixed-term contract
recode bep38 (-2 -1 3=.) (1=0) (2=1), gen(befristet)
replace befristet = 1 if bfp61==2 & syear==2015
replace befristet = 0 if bfp61==1 & syear==2015
replace befristet = 1 if bgp57==2 & syear==2016
replace befristet = 0 if bgp57==1 & syear==2016
replace befristet = 1 if bhp_60==2 & syear==2017
replace befristet = 0 if bhp_60==1 & syear==2017
replace befristet = 1 if bip_70==2 & syear==2018
replace befristet = 0 if bip_70==1 & syear==2018
replace befristet = 1 if bjp_54==2 & syear==2019
replace befristet = 0 if bjp_54==1 & syear==2019
xtset pid syear

* atypical employment
gen atyp_besch = 0 if (emplst==1 | emplst==2) & befristet!=. & zeitarbeit!=. & pgvebzeit!=.
replace atyp_besch = 1 if (befristet==1 | zeitarbeit==1 | (pgvebzeit<20 & pgvebzeit>0))
lab var atyp_besch "atypical employment"
lab def yesno 0 "no" 1 "yes"
lab val atyp_besch yesno

* prior episodes of unemployment
recode pgexpue (-3 -2 -1=.), gen(am_erf_al)
replace am_erf_al = expue19 if syear==2019 & l.pid==pid & expue19>=0

* employment tenure
recode pgerwzeit (-3 -2 -1=.), gen(tenure)
replace tenure = bjerwzeit if syear==2019 & l.pid==pid & bjerwzeit>=0

rename tenure tenure_old
gen tenure = tenure_old
replace tenure = l.tenure_old+1 if syear==2020
replace tenure = l2.tenure_old+2 if syear==2021

* employment tenure squared
gen tenure_sq = tenure*tenure

* public sector, type of job
xtset pid syear
foreach var in stib oeffd {
gen `var' = pg`var' if pg`var'>=0
replace `var' = `var'19 if syear==2019 & `var'19>=0
lab val `var' pg`var'
}

* public sector
recode oeffd (2=0), gen(public_sector)
lab var public_sector "public sector"
lab val public_sector yesno

* type of job
gen stib_kat = pgstib
replace stib_kat = stib19 if syear==2019
recode stib_kat (-1 -2/140=.) (210/250=1) (410/440=.) (510/560=2) (610/640=3)

replace stib_kat = 1 if plb0568==2 & stib_kat==.
replace stib_kat = 2 if plb0568==5 & stib_kat==.
replace stib_kat = 3 if plb0568==3 & stib_kat==.

label variable stib_kat "job type"
label define stib_kat 1 "blue collar" 2 "white collar" 3 "public servant"
label value stib_kat stib_kat

* household income
gen hh_inc = hghinc if hghinc>0
replace hh_inc = bjh_61_01 if hghinc==.
gen ln_hh_inc = ln(hh_inc)

* marital status
recode pgfamstd (-5 -3 -1=.), gen(famstand)
replace famstand = bjfamstd if syear==2019 & bjfamstd>0

recode famstand (1 2 7 8=1) (4 5=2) (2 6=3) (3=4) (7 8=5)
lab def famstand 1 "living together, married" 2 "divorced, widowed" 3 "married, living apart (incl. abroad)" 4 "single" 5 "same-sex registered civil partnership"
lab val famstand famstand

* partnership-status
gen partnersh_child = 1 if (famstand==1 | pgpartz==2 | pgpartz==4) & hlk0044==2
replace partnersh_child = 2 if (famstand==1 | famstand==5 | pgpartz==2 | pgpartz==4) & hlk0044==1
replace partnersh_child = 3 if (famstand==2 | famstand==3 | famstand==4) & pgpartz==0 & hlk0044==2
replace partnersh_child = 4 if (famstand==2 | famstand==3 | famstand==4) & pgpartz==0 & hlk0044==1
lab def partnersh_child 1 "living together without child" 2 "living together with child" 3 "living alone without child" 4 "living alone with child"
lab val partnersh_child partnersh_child

* single parent
gen single_parent = 0 if partnersh_child!=4 & partnersh_child!=.
replace single_parent = 1 if partnersh_child==4
tsset pid syear
bys pid: carryforward single_parent, gen(single_parent_cf) 
replace single_parent = 1 if single_parent_cf==1 & single_parent==. & ((famstand==2 | famstand==3 | famstand==4) & pgpartz==0)
replace single_parent = 0 if single_parent_cf==0 & single_parent==. & (famstand==1 | pgpartz==2 | pgpartz==4)
lab var single_parent "single parent"
lab val single_parent yesno

* number of children in household
gen nr_children = h11101

* religious boundaries
recode plh0258_h (-8 -5 -1=.) (1 2 3 7=1) (4=2) (5 8 9 10 11=3) (6=4), gen(relig_denom2019)
replace relig_denom2019 = . if syear!=2019
bys pid: egen relig_denom = min(relig_denom2019)
lab def relig_denom 1 "christian" 2 "islamic" 3 "other religion" 4 "no religion"
lab val relig_denom relig_denom

* legal status
gen status = 1 if (pgnation==1 | plj0722==8) | migback==1 //
replace status = 2 if plj0722==1 | plj0680_v2==8  //
replace status = 2 if plj0680_v1==4  //
replace status = 3 if 	(plj0722==2 | plj0722==3 | plj0722==4 | plj0722==5 | plj0722==7) ///
						| (plj0680_v2>0 & plj0680_v2!=. & plj0680_v2!=8 & plj0680_v2!=10) ///
						| (plj0680_v1>0 & plj0680_v1!=. & plj0680_v1!=4 & plj0680_v1!=8) //
lab def status 1 "german or EU citizen" 2 "foreigner with unlimited stay permission" 3 "foreigner with limited permission (inkl. Duldung)"
lab val status status 

bys pid: egen status_min = min(status)
replace status = status_min if status==.

* Occupations (KldB 2010)
gen kldb10_3 = int(pgkldb2010/100)	
recode kldb10_3 (0 11 12 14=.)
gen kldb10_2 = int(pgkldb2010/1000)	
recode kldb10_2 (0 1=.)
gen kldb10_1 = int(pgkldb2010/10000)	
recode kldb10_2 (0 1=.)
 
* Industries (NACE v.2)
recode bjp_49_nace2 (-2 -1=.), gen(nace)
replace nace = pgnace2 if syear!=2019 & pgnace2>0

* COVID-years
gen covidyears = 0 if syear>=2014 & syear<=2019
replace covidyears = 1 if  syear==2020 | syear==2021

* employment partner
save "$DERIVED\soepv36_soepcov_merged_temp.dta", replace

use "$SOEPV36\raw\bjpgen.dta", clear
merge 1:1 pid syear using "$SOEPV36\pgen.dta", nogen
keep if syear>=2014
replace pgemplst = emplst19 if syear==2019
rename pgemplst emplst_partner
drop pid
replace pgpartnr = partnr19 if syear==2019
rename pgpartnr pid
replace pgpartz = partz19 if syear==2019
rename pgpartz partz_partner
replace pglabgro = labgro19 if syear==2019
rename pglabgro labgro_partner
replace pgvebzeit = bjvebzeit if syear==2019
rename pgvebzeit bjvebzeit_partner
drop emplst19 partnr19 partz19 labgro19 bjvebzeit

keep pid syear emplst_partner labgro_partner bjvebzeit_partner partz_partner
drop if pid==-2

save "$DERIVED\bjpgen_partner.dta", replace

use "$DERIVED\soepv36_soepcov_merged_temp.dta", clear

merge 1:1 pid syear using "$DERIVED\bjpgen_partner.dta", gen(merge_bjpgen_partner)

xtset pid syear
replace pgpartz = partz19 if syear==2019

gen partner_erwerb = 1 if pgpartz==0
replace partner_erwerb = 2 if (emplst_partner!=1 & emplst_partner!=2) & emplst_partner!=.
replace partner_erwerb = 3 if (emplst_partner== 1) & emplst_partner!=.
replace partner_erwerb = 4 if (emplst_partner== 2) & emplst_partner!=.

lab def partner_erwerb 1 "no partner in HH" 2 "partner not ft/pt" 3 "partner ft" 4 "partner pt"
lab val partner_erwerb partner_erwerb

* household income contribution
gen increl_r_metr = pglabnet / hh_inc if hh_inc>=0 & pglabnet>=0

gen increl_cat = 1 if pgpartz==0
replace increl_cat = 2 if increl_r_metr<=0.666666666 & increl_r_metr!=. & increl_cat!=1
replace increl_cat = 3 if increl_r_metr>0.666666666 & increl_r_metr!=.	& increl_cat!=1

lab def increl_cat 1 "1 pers. HH" 2 ">2/3 of HH-inc." 3 "<=2/3 of HH-inc."
lab val increl_cat increl_cat

**********************************************************************************************************

drop if syear==2014

* Participation in SOEP-CoV and employed in 2020 and if participated in 2021 also employed in 2021 
gen cov_employed = 1 if syear==2020 & f.syear==2021 & (emplst==1 | emplst==2) & (f.emplst==1 | f.emplst==2)
replace cov_employed = 1 if syear==2020 & (emplst==1 | emplst==2) & f.syear==.
bys pid: egen cov_employed_allyears = min(cov_employed) 

*** Transferring information from previous years to 2020 and 2021
foreach var in qualifikation_isced siops08 am_erf_al atyp_besch public_sector stib_kat relig_denom single_parent nr_children increl_cat ln_hh_inc status deutsch_gesamt abschl_ausl kldb10_2 nace {
replace `var' = l.`var' if syear==2020 & l.syear==2019 & `var'==. 
replace `var' = l.`var' if l.syear==2018 & syear==2019 & `var'==. 
replace `var' = l2.`var' if l2.syear==2018 & syear==2020 & l.`var'==. & `var'==. 
replace `var' = l.`var' if l.syear==2019 & syear==2020 & `var'==.
replace `var' = l.`var' if l.syear==2020 & syear==2021 & `var'==.
replace `var' = l2.`var' if l2.syear==2019 & syear==2021 & `var'==.
}

* keeping relevant variables
keep pid syear psample sorgen_e_wirtsch fear_jobloss dir_migback migback_full qualifikation_isced female alter subjective_health am_erf_al emplst emplst2019 emplst_short siops08 public_sector stib_kat tenure tenure_sq single_parent nr_children increl_cat ln_hh_inc relig_denom status kurzarbeit atyp_besch deutsch_gesamt deutsch_gesamt_mean ysm abschl_ausl kldb10_2 nace phrf_new covidyears cov_employed cov_employed_allyears 

save "$DERIVED\ji_cov_mig_derived_final.dta", replace

display "$S_TIME  $S_DATE"
capture log close 

**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
* SAMPLE SELECTION AND DESCRIPTIVES

capture log close
log using "$LOGFILES\ji_cov_mig_descriptives_smcl.smcl", replace
display "$S_TIME  $S_DATE"

use "$DERIVED\ji_cov_mig_derived_final.dta", clear
* use "$DERIVED\soep_cov_20210421_derived_final.dta", clear

* Description without further sample selection
tab migback syear if pid!=., mis

tab migback psample if syear==2020 | syear==2021, mis
* no persons from M3 M4 M5 samples

**********************************************************************************************************
* Selection Criteria:
* 18-65 years of age 
* between 2015-2019 observed in full- or part-time (=without "in education") and observed in SOEP-CoV in 2020
* employed (FT/PT) in all observed SOEP-CoV years
* excluding self-employed individuals

keep if alter>=18 & alter<65 & pid!=.
keep if emplst_short==1 | emplst_short==3
keep if emplst2019==1 | emplst2019==2
keep if cov_employed_allyears==1
keep if stib_kat!=.
drop if migback==.
	
tab migback_full syear 

dis 2968/2982 // 2019 vs. 2020
dis 1941/2982 // 2015 vs. 2020

save "$DERIVED\ji_cov_mig_derived_final_subsample.dta", replace

**********************************************************************************************************
*** Figure 1. Developments of subjective job insecurity over time

capture ssc install cibar
cibar fear_jobloss [aweight=phrf_new] if syear==2020 | syear==2021, over(syear migback_full) ///
	bargap(3) ///
	graph(ysize(6) xsize(6) ///
	ytitle("Probability in percent") ysc(range(0(10)30)) ylabel(0 10 20 30, angle(0)) ///
	xlabel(1.5 "majority" 4.2 "second generation" 7 "first generation", labsize(medsmall)) xscale(titlegap(vsmall)) ///
	title("Probability of job loss (2020/2021)") ///
	aspectratio(1.2, placement(right))) //
graph save "$RESULTS\cibar_ji_20_21", replace

capture drop migback_timeplot
recode migback_full (2=0), gen(migback_timeplot)
qui: regress sorgen_e_wirtsch i.migback_timeplot##i.syear [pweight=phrf_new] if syear>=2015, vce(cluster pid)
qui: margins syear#migback_timeplot
qui: marginsplot, ///
	byopt(col(1)) ///
	ysc(range(0(0.2)1.4)) ysize(6) ylabel(0 `" "not concerned" "at all" "' 0.2 "0.2" 0.4 "0.4" 0.6 "0.6" 0.8 "0.8" 1 `" "somewhat" "concerned" "' 1.2 1.4, angle(0) ) ///
	xsize(14) xtitle("") ytitle("")	///
	xline(2019.5, lpattern(longdash) lcolor(gs12)) ///
	legend(col(3) order(5 "majority" 4 "second generation" 6 "first generation") symx(6)) ///
	title("Financial worries (2015-2021)", size(large) xoffset(12)) ///
	plot1opts(lpattern(shortdash) msymbol(circle hollow) mcolor(gs10) lcolor(gs12)) ci1opts(lcolor(gs12)) ///
	plot2opts(lpattern(solid) msymbol(diamond) mcolor(black) lcolor(black)) ci2opts(lcolor(black)) ///
	plot3opts(lpattern(dashdot) msymbol(square) mcolor(gs6) lcolor(gs6)) ci3opts(lcolor(gs6)) //
graph save "$RESULTS\margins_time_worries_15_21", replace
drop migback_timeplot
tab migback_full syear if e(sample)

graph combine "$RESULTS\cibar_ji_20_21" "$RESULTS\margins_time_worries_15_21" , cols(2) xsize(16) ysize(6)
graph save "$RESULTS\figure1.gph", replace
graph export "$RESULTS\figure1.pdf", as(pdf) replace
graph export "$RESULTS\figure1.svg", as(svg) replace

preserve 
format fear_jobloss %4.1f
format sorgen_e_wirtsch %4.2f

tab migback syear [aweight=phrf_new], sum(fear_jobloss)

dis 10.7-6.9 // absolute difference majority 2020 to 2021
dis 18.5-13.2 // absolute difference generation 2020 to 2021

dis (10.7-6.9)/10.7 // relative difference majority 2020 to 2021
dis (18.5-13.2)/18.5 // relative change first generation 2020 to 2021

tab fear_jobloss sorgen_e_wirtsch, V chi2

tab syear migback  [aweight=phrf_new], sum(sorgen_e_wirtsch)

dis 0.51-0.79 // absolute difference majority vs first generation in 2020
dis 0.42-0.69 // absolute difference majority vs first generation in 2021

dis (0.51-0.79)/0.79 // relative difference majority vs first generation in 2020
dis (0.42-0.69)/0.69 // relative difference majority vs first generation in 2021
restore 

capture log close

**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************
* MODELS

capture log close
log using "$LOGFILES\ji_cov_mig_models_smcl.smcl", replace
use "$DERIVED\ji_cov_mig_derived_final_subsample.dta", clear

foreach var in migback_full qualifikation_isced stib_kat relig_denom increl_cat syear status {
tab `var', gen(`var'_d)
}
gen emplst_dum = emplst-1

recode syear (2021=2020), gen(syear_new)
gen kldb10_1 = int(kldb10_2/10)	
gen nace_1 = int(nace/10)	

**********************************************************************************************************
*** All Respondents: Linear regressions, fear of job loss, 2020/2021 (M1a & M2a)

global UVS_RAW "i.migback_full i.female alter i.qualifikation_isced c.subjective_health c.siops08 i.atyp_besch c.am_erf_al c.tenure c.tenure_sq i.public_sector i.stib_kat i.emplst i.kurzarbeit c.ln_hh_inc i.increl_cat i.single_parent c.nr_children i.relig_denom"

* M1a
reg fear_jobloss $UVS_RAW i.syear, vce(cl pid)
est store jobloss_m1

* Description M1a	
estpost sum fear_jobloss migback_full_d* female alter qualifikation_isced_d* subjective_health siops08 atyp_besch am_erf_al tenure public_sector stib_kat_d* emplst_dum kurzarbeit relig_denom_d* single_parent nr_children  ln_hh_inc increl_cat_d* syear_d* if e(sample)
esttab using "$RESULTS\descriptives_M1a", cells("mean(fmt(2)) sd(fmt(2)) count(fmt(2))" ) noobs rtf replace

* M2a
reg fear_jobloss $UVS_RAW i.syear i.kldb10_2 i.nace, vce(cl pid)
est store jobloss_m2

esttab jobloss_m1 jobloss_m2 using "$RESULTS\ols_jobloss_m1a_m2a.rtf", r2 wide b(2) not replace noomit nobase mtitle

	* check different FE-specifications	
	reg fear_jobloss $UVS_RAW i.syear i.kldb10_1, vce(cl pid)
	est store kldb_1
	reg fear_jobloss $UVS_RAW i.syear i.nace_1, vce(cl pid)
	est store nace_1
	reg fear_jobloss $UVS_RAW i.syear i.kldb10_2, vce(cl pid)
	est store kldb_2
	reg fear_jobloss $UVS_RAW i.syear i.nace, vce(cl pid)
	est store nace_2
	
	esttab jobloss_m1 kldb_1 kldb_2 nace_1 nace_2 jobloss_m2 using "$RESULTS\ols_jobloss_m2a_checks.rtf", r2 wide b(2) not replace noomit nobase mtitle

**********************************************************************************************************
*** First generation: Linear regressions, fear of job loss, 2020/2021 (M1b & M2b)

global UVS_RAW_MIG "ysm i.female alter i.qualifikation_isced i.abschl_ausl deutsch_gesamt c.subjective_health c.siops08 i.atyp_besch c.am_erf_al c.tenure c.tenure_sq i.public_sector i.stib_kat i.emplst i.kurzarbeit c.ln_hh_inc i.increl_cat i.single_parent c.nr_children i.status i.relig_denom"

* M1b
reg fear_jobloss $UVS_RAW_MIG i.syear if dir_migback==1, vce(cl pid)
est store jobloss_m1_mig
	
* Description M1b	
estpost sum fear_jobloss ysm female alter qualifikation_isced_d* abschl_ausl deutsch_gesamt subjective_health siops08 atyp_besch am_erf_al tenure public_sector  stib_kat_d* emplst_dum kurzarbeit ln_hh_inc increl_cat_d* single_parent nr_children relig_denom_d* status_d* syear_d* if e(sample)
esttab using "$RESULTS\descriptives_M1b", cells("mean(fmt(2)) sd(fmt(2)) count(fmt(2))" ) noobs rtf replace

* M2b
reg fear_jobloss $UVS_RAW_MIG i.syear i.kldb10_2 i.nace if dir_migback==1, vce(cl pid)
est store jobloss_m2_mig

esttab jobloss_m1_mig jobloss_m2_mig using "$RESULTS\ols_jobloss_m1b_m2b.rtf", r2 wide b(2) not replace noomit nobase mtitle

	* check different FE-specifications	
	reg fear_jobloss $UVS_RAW_MIG i.syear i.kldb10_1, vce(cl pid)
	est store kldb_1_mig
	reg fear_jobloss $UVS_RAW_MIG i.syear i.nace_1, vce(cl pid)
	est store nace_1_mig
	reg fear_jobloss $UVS_RAW_MIG i.syear i.kldb10_2, vce(cl pid)
	est store kldb_2_mig
	reg fear_jobloss $UVS_RAW_MIG i.syear i.nace, vce(cl pid)
	est store nace_2_mig
	reg fear_jobloss $UVS_RAW_MIG i.syear i.nace_1 i.kldb10_1, vce(cl pid)
	est store nacekldb_1_mig

	esttab jobloss_m1_mig kldb_1 kldb_2 nace_1 nace_2 jobloss_m2_mig , mtitle

	esttab  nace_1_mig nace_2_mig kldb_1_mig kldb_2_mig nacekldb_1_mig using "$RESULTS\ols_jobloss_m2b_checks.rtf", r2 wide b(2) not replace noomit nobase mtitle

**********************************************************************************************************
*** All Respondents: Linear regressions, financial worries, 2015-2021 (M3a & M4a)

foreach var in female atyp_besch public_sector single_parent abschl_ausl {
rename `var' `var'_dum
gen `var' = `var'_dum+1
}

foreach var in migback_full female alter subjective_health qualifikation_isced emplst siops08 public_sector stib_kat am_erf_al atyp_besch tenure tenure_sq relig_denom single_parent nr_children increl_cat ln_hh_inc {
gen `var'_main = `var' if `var'!=.
gen `var'_ia = 0 if covidyears==0 & `var'!=.
replace `var'_ia = `var' if covidyears==1
}

global UVS_MAIN "ib(#1).migback_full_main ib(#1).female_main alter_main ib(#1).qualifikation_isced_main c.subjective_health_main c.siops08_main ib(#1).atyp_besch_main c.am_erf_al_main c.tenure_main c.tenure_sq_main ib(#1).public_sector_main ib(#1).stib_kat_main ib(#1).emplst_main c.ln_hh_inc_main ib(#1).increl_cat_main ib(#1).single_parent_main c.nr_children_main ib(#1).relig_denom_main"
global UVS_IA "ib(#2).migback_full_ia ib(#2).female_ia alter_ia ib(#2).qualifikation_isced_ia c.subjective_health_ia c.siops08_ia ib(#2).atyp_besch_ia c.am_erf_al_ia c.tenure_ia c.tenure_sq_ia ib(#2).public_sector_ia ib(#2).stib_kat_ia ib(#2).emplst_ia c.ln_hh_inc_ia ib(#2).increl_cat_ia ib(#2).single_parent_ia c.nr_children_ia ib(#2).relig_denom_ia"

* M3a
reg sorgen_e_wirtsch i.covidyears $UVS_MAIN $UVS_IA i.syear, vce(cl pid)
est store worries_m3

* Description for model M3a
estpost sum sorgen_e_wirtsch migback_full_d* female_dum alter qualifikation_isced_d* subjective_health siops08 atyp_besch_dum am_erf_al tenure public_sector_dum stib_kat_d* emplst_dum relig_denom_d* single_parent_dum nr_children increl_cat_d* ln_hh_inc syear_d* if e(sample)
esttab using "$RESULTS\descriptives_M3a", cells("mean(fmt(2)) sd(fmt(2)) count(fmt(2))" ) noobs rtf replace

* M4a
reg sorgen_e_wirtsch i.covidyears $UVS_MAIN $UVS_IA i.syear i.kldb10_2 i.nace, vce(cl pid)
est store worries_m4

esttab worries_m3 worries_m4 using "$RESULTS\ols_worries_m3a_m4a.rtf", r2 wide b(2) not replace noomit nobase mtitle

	* test seperate models
	reg sorgen_e_wirtsch $UVS_MAIN i.syear if syear<=2019, vce(cl pid)
	est store worries_m3_1519
	reg sorgen_e_wirtsch $UVS_MAIN i.syear if syear>2019, vce(cl pid)
	est store worries_m3_2021
	reg sorgen_e_wirtsch $UVS_MAIN i.syear i.kldb10_2 i.nace if syear<=2019, vce(cl pid)
	est store worries_m4_1519
	reg sorgen_e_wirtsch $UVS_MAIN i.syear i.kldb10_2 i.nace if syear>2019, vce(cl pid)
	est store worries_m4_2021

	* test ologit
	ologit sorgen_e_wirtsch i.covidyears $UVS_MAIN $UVS_IA i.syear, vce(cl pid)
	est store worries_m3_ologit

	* comparison with ologit & separate models
	esttab worries_m3_1519 worries_m3_2021 worries_m3 worries_m3_ologit using "$RESULTS\ols_worries_m3a_checks.rtf", r2 wide b(2) not replace noomit nobase mtitle

drop migback_full_main migback_full_ia female_main female_ia alter_main alter_ia subjective_health_main subjective_health_ia qualifikation_isced_main qualifikation_isced_ia emplst_main emplst_ia siops08_main siops08_ia public_sector_main public_sector_ia stib_kat_main stib_kat_ia am_erf_al_main am_erf_al_ia atyp_besch_main atyp_besch_ia tenure_main tenure_ia tenure_sq_main tenure_sq_ia relig_denom_main relig_denom_ia single_parent_main single_parent_ia nr_children_main nr_children_ia increl_cat_main increl_cat_ia ln_hh_inc_main ln_hh_inc_ia

**********************************************************************************************************
*** First generation: Linear regressions, financial worries, 2015-2021 (M3b & M4b)

foreach var in migback_full female alter subjective_health qualifikation_isced emplst siops08 public_sector stib_kat am_erf_al atyp_besch tenure tenure_sq relig_denom single_parent nr_children increl_cat ln_hh_inc deutsch_gesamt ysm status abschl_ausl {
gen `var'_main = `var' if `var'!=.
gen `var'_ia = 0 if covidyears==0 & `var'!=.
replace `var'_ia = `var' if covidyears==1
}

global UVS_MAIN_MIG "ysm_main ib(#1).female_main alter_main ib(#1).qualifikation_isced_main ib(#1).abschl_ausl_main deutsch_gesamt_main c.subjective_health_main c.siops08_main ib(#1).atyp_besch_main c.am_erf_al_main c.tenure_main c.tenure_sq_main ib(#1).public_sector_main ib(#1).stib_kat_main ib(#1).emplst_main c.ln_hh_inc_main ib(#1).increl_cat_main ib(#1).single_parent_main c.nr_children_main ib(#1).status_main ib(#1).relig_denom_main"
global UVS_IA_MIG "ysm_ia ib(#2).female_ia alter_ia ib(#2).qualifikation_isced_ia ib(#1).abschl_ausl_ia deutsch_gesamt_ia c.subjective_health_ia c.siops08_ia ib(#2).atyp_besch_ia c.am_erf_al_ia c.tenure_ia c.tenure_sq_ia ib(#2).public_sector_ia ib(#2).stib_kat_ia ib(#2).emplst_ia c.ln_hh_inc_ia ib(#2).increl_cat_ia ib(#2).single_parent_ia c.nr_children_ia ib(#2).status_ia ib(#2).relig_denom_ia"

* M3b
reg sorgen_e_wirtsch i.covidyears $UVS_MAIN_MIG $UVS_IA_MIG i.syear if dir_migback==1, vce(cl pid)
est store worries_m3_mig

* Description for model M3b
estpost sum sorgen_e_wirtsch ysm female_dum alter qualifikation_isced_d* abschl_ausl_dum deutsch_gesamt subjective_health siops08 atyp_besch_dum am_erf_al tenure public_sector_dum  stib_kat_d* emplst_dum ln_hh_inc increl_cat_d* single_parent_dum nr_children relig_denom_d* status_d* syear_d* if e(sample)
esttab using "$RESULTS\descriptives_M3b", cells("mean(fmt(2)) sd(fmt(2)) count(fmt(2))" ) noobs rtf replace

* M4b
reg sorgen_e_wirtsch i.covidyears $UVS_MAIN_MIG $UVS_IA_MIG i.syear i.kldb10_2 i.nace if dir_migback==1, vce(cl pid)
est store worries_m4_mig

esttab worries_m3_mig worries_m4_mig using "$RESULTS\ols_worries_m3b_m4b.rtf", r2 wide b(2) not replace noomit nobase mtitle

	* test seperate models
	reg sorgen_e_wirtsch $UVS_MAIN_MIG i.syear if syear<=2019, vce(cl pid)
	est store worries_m3_1519_mig
	reg sorgen_e_wirtsch $UVS_MAIN_MIG i.syear if syear>2019, vce(cl pid)
	est store worries_m3_2021_mig
	
	* test ologit
	ologit sorgen_e_wirtsch i.covidyears $UVS_MAIN_MIG $UVS_IA_MIG i.syear, vce(cl pid)
	est store worries_m3_ologit_mig

	* comparison with ologit & separate models
	esttab worries_m3_1519_mig worries_m3_2021_mig worries_m3_mig worries_m3_ologit_mig using "$RESULTS\ols_worries_m3b_checks.rtf", r2 wide b(2) not replace noomit nobase mtitle

drop migback_full_main migback_full_ia female_main female_ia alter_main alter_ia subjective_health_main subjective_health_ia qualifikation_isced_main qualifikation_isced_ia emplst_main emplst_ia siops08_main siops08_ia public_sector_main public_sector_ia stib_kat_main stib_kat_ia am_erf_al_main am_erf_al_ia atyp_besch_main atyp_besch_ia tenure_main tenure_ia tenure_sq_main tenure_sq_ia relig_denom_main relig_denom_ia single_parent_main single_parent_ia nr_children_main nr_children_ia increl_cat_main increl_cat_ia ln_hh_inc_main ln_hh_inc_ia deutsch_gesamt_main deutsch_gesamt_ia ysm_main ysm_ia status_main status_ia abschl_ausl_main abschl_ausl_ia

log close

**********************************************************************************************************
**********************************************************************************************************
**********************************************************************************************************