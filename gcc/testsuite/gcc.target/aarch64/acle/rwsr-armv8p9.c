/* Ensure support is present for all armv8.9-a system registers.  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.9-a" } */
#include <arm_acle.h>
void
readwrite_armv8p9a_sysregs (long long int a)
{
  /* Write-only system registers.  */
  __arm_wsr64 ("pmzr_el0", a); /* { dg-final { scan-assembler "msr\ts3_3_c9_c13_4, x0" } } */

  /* Read/write or write-only system registers.  */
  a = __arm_rsr64 ("amair2_el1");		/* { { dg-final { scan-assembler "s3_0_c10_c3_1" } } */
  a = __arm_rsr64 ("amair2_el12"); /* { { dg-final { scan-assembler "mrs\tx0, s3_5_c10_c3_1" } } */
  a = __arm_rsr64 ("amair2_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c10_c3_1" } } */
  a = __arm_rsr64 ("amair2_el3"); /* { { dg-final { scan-assembler "mrs\tx0, s3_6_c10_c3_1" } } */
  a = __arm_rsr64 ("erxgsr_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c5_c3_2" } } */
  a = __arm_rsr64 ("hdfgrtr2_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c3_c1_0" } } */
  a = __arm_rsr64 ("hdfgwtr2_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c3_c1_1" } } */
  a = __arm_rsr64 ("hfgrtr2_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c3_c1_2" } } */
  a = __arm_rsr64 ("hfgwtr2_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c3_c1_3" } } */
  a = __arm_rsr64 ("id_aa64mmfr3_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c0_c7_3" } } */
  a = __arm_rsr64 ("id_aa64mmfr4_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c0_c7_4" } } */
  a = __arm_rsr64 ("mair2_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c10_c2_1" } } */
  a = __arm_rsr64 ("mair2_el12"); /* { { dg-final { scan-assembler "mrs\tx0, s3_5_c10_c2_1" } } */
  a = __arm_rsr64 ("mair2_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c10_c1_1" } } */
  a = __arm_rsr64 ("mair2_el3"); /* { { dg-final { scan-assembler "mrs\tx0, s3_6_c10_c1_1" } } */
  a = __arm_rsr64 ("mdselr_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c0_c4_2" } } */
  a = __arm_rsr64 ("pir_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c10_c2_3" } } */
  a = __arm_rsr64 ("pir_el12"); /* { { dg-final { scan-assembler "mrs\tx0, s3_5_c10_c2_3" } } */
  a = __arm_rsr64 ("pir_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c10_c2_3" } } */
  a = __arm_rsr64 ("pir_el3"); /* { { dg-final { scan-assembler "mrs\tx0, s3_6_c10_c2_3" } } */
  a = __arm_rsr64 ("pire0_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c10_c2_2" } } */
  a = __arm_rsr64 ("pire0_el12"); /* { { dg-final { scan-assembler "mrs\tx0, s3_5_c10_c2_2" } } */
  a = __arm_rsr64 ("pire0_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c10_c2_2" } } */
  a = __arm_rsr64 ("pfar_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c6_c0_5" } } */
  a = __arm_rsr64 ("pfar_el12"); /* { { dg-final { scan-assembler "mrs\tx0, s3_5_c6_c0_5" } } */
  a = __arm_rsr64 ("pfar_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c6_c0_5" } } */
  a = __arm_rsr64 ("pmccntsvr_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c11_7" } } */
  a = __arm_rsr64 ("pmecr_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c9_c14_5" } } */
  a = __arm_rsr64 ("pmevcntsvr0_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c8_0" } } */
  a = __arm_rsr64 ("pmevcntsvr10_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c9_2" } } */
  a = __arm_rsr64 ("pmevcntsvr11_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c9_3" } } */
  a = __arm_rsr64 ("pmevcntsvr12_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c9_4" } } */
  a = __arm_rsr64 ("pmevcntsvr13_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c9_5" } } */
  a = __arm_rsr64 ("pmevcntsvr14_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c9_6" } } */
  a = __arm_rsr64 ("pmevcntsvr15_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c9_7" } } */
  a = __arm_rsr64 ("pmevcntsvr16_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c10_0" } } */
  a = __arm_rsr64 ("pmevcntsvr17_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c10_1" } } */
  a = __arm_rsr64 ("pmevcntsvr18_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c10_2" } } */
  a = __arm_rsr64 ("pmevcntsvr19_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c10_3" } } */
  a = __arm_rsr64 ("pmevcntsvr1_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c8_1" } } */
  a = __arm_rsr64 ("pmevcntsvr20_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c10_4" } } */
  a = __arm_rsr64 ("pmevcntsvr21_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c10_5" } } */
  a = __arm_rsr64 ("pmevcntsvr22_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c10_6" } } */
  a = __arm_rsr64 ("pmevcntsvr23_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c10_7" } } */
  a = __arm_rsr64 ("pmevcntsvr24_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c11_0" } } */
  a = __arm_rsr64 ("pmevcntsvr25_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c11_1" } } */
  a = __arm_rsr64 ("pmevcntsvr26_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c11_2" } } */
  a = __arm_rsr64 ("pmevcntsvr27_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c11_3" } } */
  a = __arm_rsr64 ("pmevcntsvr28_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c11_4" } } */
  a = __arm_rsr64 ("pmevcntsvr29_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c11_5" } } */
  a = __arm_rsr64 ("pmevcntsvr2_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c8_2" } } */
  a = __arm_rsr64 ("pmevcntsvr30_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c11_6" } } */
  a = __arm_rsr64 ("pmevcntsvr3_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c8_3" } } */
  a = __arm_rsr64 ("pmevcntsvr4_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c8_4" } } */
  a = __arm_rsr64 ("pmevcntsvr5_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c8_5" } } */
  a = __arm_rsr64 ("pmevcntsvr6_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c8_6" } } */
  a = __arm_rsr64 ("pmevcntsvr7_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c8_7" } } */
  a = __arm_rsr64 ("pmevcntsvr8_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c9_0" } } */
  a = __arm_rsr64 ("pmevcntsvr9_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c9_1" } } */
  a = __arm_rsr64 ("pmiar_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c9_c14_7" } } */
  a = __arm_rsr64 ("pmicfiltr_el0"); /* { { dg-final { scan-assembler "mrs\tx0, s3_3_c9_c6_0" } } */
  a = __arm_rsr64 ("pmicntr_el0"); /* { { dg-final { scan-assembler "mrs\tx0, s3_3_c9_c4_0" } } */
  a = __arm_rsr64 ("pmicntsvr_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s2_0_c14_c12_0" } } */
  a = __arm_rsr64 ("pmsdsfr_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c9_c10_4" } } */
  a = __arm_rsr64 ("pmsscr_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c9_c13_3" } } */
  a = __arm_rsr64 ("pmuacr_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c9_c14_4" } } */
  a = __arm_rsr64 ("por_el0"); /* { { dg-final { scan-assembler "mrs\tx0, s3_3_c10_c2_4" } } */
  a = __arm_rsr64 ("por_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c10_c2_4" } } */
  a = __arm_rsr64 ("por_el12"); /* { { dg-final { scan-assembler "mrs\tx0, s3_5_c10_c2_4" } } */
  a = __arm_rsr64 ("por_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c10_c2_4" } } */
  a = __arm_rsr64 ("por_el3"); /* { { dg-final { scan-assembler "mrs\tx0, s3_6_c10_c2_4" } } */
  a = __arm_rsr64 ("sctlr2_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c1_c0_3" } } */
  a = __arm_rsr64 ("sctlr2_el12"); /* { { dg-final { scan-assembler "mrs\tx0, s3_5_c1_c0_3" } } */
  a = __arm_rsr64 ("sctlr2_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c1_c0_3" } } */
  a = __arm_rsr64 ("sctlr2_el3"); /* { { dg-final { scan-assembler "mrs\tx0, s3_6_c1_c0_3" } } */
  a = __arm_rsr64 ("s2pir_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c10_c2_5" } } */
  a = __arm_rsr64 ("s2por_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c10_c2_5" } } */
  a = __arm_rsr64 ("tcr2_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c2_c0_3" } } */
  a = __arm_rsr64 ("tcr2_el12"); /* { { dg-final { scan-assembler "mrs\tx0, s3_5_c2_c0_3" } } */
  a = __arm_rsr64 ("tcr2_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c2_c0_3" } } */
  a = __arm_rsr64 ("trcitecr_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c1_c2_3" } } */
  a = __arm_rsr64 ("trcitecr_el12"); /* { { dg-final { scan-assembler "mrs\tx0, s3_5_c1_c2_3" } } */
  a = __arm_rsr64 ("trcitecr_el2"); /* { { dg-final { scan-assembler "mrs\tx0, s3_4_c1_c2_3" } } */
  a = __arm_rsr64 ("trciteedcr"); /* { { dg-final { scan-assembler "mrs\tx0, s2_1_c0_c2_1" } } */
}

