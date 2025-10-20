/* { dg-do compile } */
/* { dg-options "-menable-sysreg-checking -march=armv8-a+sve2+sme" } */
/* Ensure that system registers are properly gated on the feature flags, when
   the guarding is enabled through "-menable-sysreg-checking" command line
   flag.  */

#include <arm_acle.h>

uint64_t
foo (uint64_t a)
{
  __arm_wsr64 ("zcr_el1", a); /* { { dg-final { scan-assembler "msr\ts3_0_c1_c2_0, x0" } } */
  return __arm_rsr64 ("smcr_el1"); /* { { dg-final { scan-assembler "mrs\tx0, s3_0_c1_c2_6" } } */
}
