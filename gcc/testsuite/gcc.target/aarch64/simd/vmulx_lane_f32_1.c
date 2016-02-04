/* Test the vmulx_lane_f32 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

float32x2_t __attribute__ ((noinline))
test_vmulx_lane0_f32 (float32x2_t vec1_1, float32x2_t vec1_2)
{
  return vmulx_lane_f32 (vec1_1, vec1_2, 0);
}

float32x2_t __attribute__ ((noinline))
test_vmulx_lane1_f32 (float32x2_t vec1_1, float32x2_t vec1_2)
{
  return vmulx_lane_f32 (vec1_1, vec1_2, 1);
}

void
test_case (float32_t v1[2], float32_t v2[2], float32_t e1[2], float32_t e2[2])
{
  int i;
  float32x2_t vec1_1 = vld1_f32 (v1);
  float32x2_t vec1_2 = vld1_f32 (v2);


  float32x2_t actual1 = test_vmulx_lane0_f32 (vec1_1, vec1_2);
  float32_t actual1_1[2];
  vst1_f32 (actual1_1, actual1);

  for (i = 0; i < 2; ++i)
    if (actual1_1[i] != e1[i])
      abort ();

  float32x2_t actual2 = test_vmulx_lane1_f32 (vec1_1, vec1_2);
  float32_t actual2_1[2];
  vst1_f32 (actual2_1, actual2);

  for (i = 0; i < 2; ++i)
    if (actual2_1[i] != e2[i])
      abort ();
}

int
main (void)
{
  float32_t v1 = 3.14159265359;
  float32_t v2 = 1.383894;
  float32_t v3 = -2.71828;
  float32_t v4 = -3.4891931;

  float32_t v1_1[] = {v1, v2};
  float32_t v1_2[] = {v3, v4};
  float32_t e1_1[] = {v1 * v3, v2 * v3};
  float32_t e1_2[] = {v1 * v4, v2 * v4};
  test_case (v1_1, v1_2, e1_1, e1_2);

  float32_t v2_1[] = {0, -0.0};
  float32_t v2_2[] = {__builtin_huge_valf (), -__builtin_huge_valf ()};
  float32_t e2_1[] = {2.0, -2.0};
  float32_t e2_2[] = {-2.0, 2.0};
  test_case (v2_1, v2_2, e2_1, e2_2);

  return 0;
}
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[0\\\]\n" 1 } } */
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[1\\\]\n" 1 } } */
