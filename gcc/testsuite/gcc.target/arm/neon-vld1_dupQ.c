/* Test the `vld1q_s64' ARM Neon intrinsic.  */

/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O0" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include <stdlib.h>

int main (void)
{
  int64x1_t input[2] = {(int64x1_t)0x0123456776543210LL,
			(int64x1_t)0x89abcdeffedcba90LL};
  int64x1_t output[2] = {0, 0};
  int64x2_t var = vld1q_dup_s64((int64_t *)input);

  vst1q_s64((int64_t *)output, var);
  if (output[0] != (int64x1_t)0x0123456776543210LL)
    abort();
  if (output[1] != (int64x1_t)0x0123456776543210LL)
    abort();
  return 0;
}
