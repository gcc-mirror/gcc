/* Test the vmulxq_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

void  __attribute__ ((noinline))
test_case (float64_t v1[2], float64_t v2[2], float64_t e1[2])
{
  int i;
  float64x2_t vec1_1 = vld1q_f64 (v1);
  float64x2_t vec1_2 = vld1q_f64 (v2);

  float64x2_t actual1 = vmulxq_f64 (vec1_1, vec1_2);
  float64_t actual[2];
  vst1q_f64 (actual, actual1);

  for (i = 0; i < 2; ++i)
    if (actual[i] != e1[i])
      abort ();
}

int
main (void)
{
  int i;
  float64_t v1 = 3.14159265359;
  float64_t v2 = -2.71828;

  float64_t v1_1[] = {v1, v2};
  float64_t v1_2[] = {v2, v1};
  float64_t e1[] = {v1 * v2, v2* v1};
  test_case (v1_1, v1_2, e1);

  float64_t v2_1[] = {0, 0};
  float64_t v2_2[] = {__builtin_huge_val (), -__builtin_huge_val ()};
  float64_t e2[] = {2.0, -2.0};
  test_case (v2_1, v2_2, e2);

  float64_t v3_1[] = {-0.0, -0.0};
  float64_t v3_2[] = {__builtin_huge_val (), -__builtin_huge_val ()};
  float64_t e3[] = {-2.0, 2.0};
  test_case (v3_1, v3_2, e3);

  return 0;
}

/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[vV\]\[0-9\]+\.2\[dD\], ?\[vV\]\[0-9\]+\.2\[dD\], ?\[vV\]\[0-9\]+\.2\[dD\]\n" 1} } */
