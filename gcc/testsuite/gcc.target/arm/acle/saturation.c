/* { dg-do compile } */
/* { dg-require-effective-target arm_qbit_ok } */
/* { dg-add-options arm_qbit } */

#include <arm_acle.h>

int32_t
test_ssat (int32_t a)
{
  return __ssat (a, 8);
}

/* { dg-final { scan-assembler-times "ssat\t...?, #8, ...?" 1 } } */

uint32_t
test_usat (int32_t a)
{
  return __usat (a, 24);
}

/* { dg-final { scan-assembler-times "usat\t...?, #24, ...?" 1 } } */

/* Test that USAT doesn't get removed as we need its Q-setting behavior.  */
int
test_sat_occur (int32_t a)
{
  uint32_t res = __usat (a, 3);
  return __saturation_occurred ();
}

/* { dg-final { scan-assembler-times "usat\t...?, #3, ...?" 1 } } */
/* { dg-final { scan-assembler "mrs\t...?, APSR" } } */

void
test_set_sat (void)
{
  __set_saturation_occurred (0);
}

/* { dg-final { scan-assembler-times "msr\tAPSR_nzcvq, ...?" 1 } } */
