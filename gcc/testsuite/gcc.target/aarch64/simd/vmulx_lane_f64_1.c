/* Test the vmulx_lane_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

float64x1_t __attribute__ ((noinline))
test_vmulx_lane_f64 (float64x1_t vec1_1, float64x1_t vec1_2)
{
  return vmulx_lane_f64 (vec1_1, vec1_2, 0);
}

void
test_case (float64_t v1[], float64_t v2[], float64_t e[])
{
  float64x1_t vec1_1 = vld1_f64 (v1);
  float64x1_t vec1_2 = vld1_f64 (v2);
  float64x1_t expected1 = vld1_f64 (e);

  float64x1_t actual1 = test_vmulx_lane_f64 (vec1_1, vec1_2);
  float64_t actual[1];
  vst1_f64 (actual, actual1);
  if (actual[0] != e[0])
    abort ();
}
int
main (void)
{
  float64_t v1 = 3.14159265359;
  float64_t v2 = -2.71828;

  float64_t v1_1[] = {v1};
  float64_t v1_2[] =  {v2};
  float64_t e1[] = {v1 * v2};
  test_case (v1_1, v1_2, e1);

  float64_t v2_1[] = {0};
  float64_t v2_2[] = {__builtin_huge_val ()};
  float64_t e2[] = {2.0};
  test_case (v2_1, v2_2, e2);

  float64_t v4_1[] = {0};
  float64_t v4_2[] = {-__builtin_huge_val ()};
  float64_t e4[] = {-2.0};
  test_case (v4_1, v4_2, e4);

  float64_t v5_1[] = {-0.0};
  float64_t v5_2[] = {__builtin_huge_val ()};
  float64_t e5[] = {-2.0};
  test_case (v5_1, v5_2, e5);

  float64_t v6_1[] = {-0.0};
  float64_t v6_2[] = {-__builtin_huge_val ()};
  float64_t e6[] = {2.0};
  test_case (v6_1, v6_2, e6);

  return 0;
}
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" 1 } } */
