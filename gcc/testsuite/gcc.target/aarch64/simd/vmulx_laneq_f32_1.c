/* Test the vmulx_laneq_f32 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

float32x2_t __attribute__ ((noinline))
test_vmulx_laneq_f32_lane0 (float32x2_t vec1_1, float32x4_t vec1_2)
{
  return vmulx_laneq_f32 (vec1_1, vec1_2, 0);
}

float32x2_t __attribute__ ((noinline))
test_vmulx_laneq_f32_lane1 (float32x2_t vec1_1, float32x4_t vec1_2)
{
  return vmulx_laneq_f32 (vec1_1, vec1_2, 1);
}

float32x2_t __attribute__ ((noinline))
test_vmulx_laneq_f32_lane2 (float32x2_t vec1_1, float32x4_t vec1_2)
{
  return vmulx_laneq_f32 (vec1_1, vec1_2, 2);
}

float32x2_t __attribute__ ((noinline))
test_vmulx_laneq_f32_lane3 (float32x2_t vec1_1, float32x4_t vec1_2)
{
  return vmulx_laneq_f32 (vec1_1, vec1_2, 3);
}

#define PASS_ARRAY(...) {__VA_ARGS__}

#define SETUP_VEC(V1_D, V2_D, EXP0, EXP1, EXP2, EXP3, I)		\
  void set_and_test_case##I ()						\
  {									\
    float32_t vec1_data[] = V1_D;					\
    float32x2_t vec1 = vld1_f32 (vec1_data);				\
    float32_t vec2_data[] =  V2_D;					\
    float32x4_t vec2 = vld1q_f32 (vec2_data);				\
									\
    float32_t expected_lane0[] = EXP0;					\
    float32_t expected_lane1[] = EXP1;					\
    float32_t expected_lane2[] = EXP2;					\
    float32_t expected_lane3[] = EXP3;					\
									\
    float32x2_t actual_lane0_v =					\
      test_vmulx_laneq_f32_lane0 (vec1, vec2);				\
    float32_t actual_lane0[2];						\
    vst1_f32 (actual_lane0, actual_lane0_v);				\
    if (actual_lane0[0] != expected_lane0[0]				\
	|| actual_lane0[1] != expected_lane0[1])			\
      abort ();								\
									\
    float32x2_t actual_lane1_v =					\
      test_vmulx_laneq_f32_lane1 (vec1, vec2);				\
    float32_t actual_lane1[2];						\
    vst1_f32 (actual_lane1, actual_lane1_v);				\
    if (actual_lane1[0] != expected_lane1[0]				\
	|| actual_lane1[1] != expected_lane1[1])			\
      abort ();								\
									\
    float32x2_t actual_lane2_v =					\
      test_vmulx_laneq_f32_lane2 (vec1, vec2);				\
    float32_t actual_lane2[2];						\
    vst1_f32 (actual_lane2, actual_lane2_v);				\
    if (actual_lane2[0] != expected_lane2[0]				\
	|| actual_lane2[1] != expected_lane2[1])			\
      abort ();								\
									\
    float32x2_t actual_lane3_v =					\
      test_vmulx_laneq_f32_lane3 (vec1, vec2);				\
    float32_t actual_lane3[2];						\
    vst1_f32 (actual_lane3, actual_lane3_v);				\
    if (actual_lane3[0] != expected_lane3[0]				\
	|| actual_lane3[1] != expected_lane3[1])			\
      abort ();								\
									\
  }									\

float32_t v1 = 3.14159265359;
float32_t v2 = 1.383894;
float32_t v3 = -2.71828;
float32_t v4 = -3.4891931;

float32_t v5 = 0.0;
float32_t v6 = -0.0;
float32_t v7 = __builtin_huge_valf ();
float32_t v8 = -__builtin_huge_valf ();

SETUP_VEC (PASS_ARRAY (v1, v2), PASS_ARRAY (v1, v2, v3, v4),
	   PASS_ARRAY (v1*v1, v1*v2), PASS_ARRAY (v1*v2, v2*v2),
	   PASS_ARRAY (v1*v3, v2*v3), PASS_ARRAY (v1*v4, v2*v4), 1)

SETUP_VEC (PASS_ARRAY (v5, v6), PASS_ARRAY (v5, v6, v7, v8),
	   PASS_ARRAY (0.0, -0.0), PASS_ARRAY (-0.0, 0.0),
	   PASS_ARRAY (2.0, -2.0), PASS_ARRAY (-2.0, 2.0), 2)

int
main (void)
{
  set_and_test_case1 ();
  set_and_test_case2 ();
  return 0;
}
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[0\\\]\n" 1 } } */
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[1\\\]\n" 1 } } */
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[2\\\]\n" 1 } } */
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[3\\\]\n" 1 } } */
