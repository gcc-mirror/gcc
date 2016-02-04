/* Test the vmulxq_laneq_f32 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

float32x4_t __attribute__ ((noinline))
test_vmulxq_laneq_f32_lane0 (float32x4_t vec1_1, float32x4_t vec1_2)
{
  return vmulxq_laneq_f32 (vec1_1, vec1_2, 0);
}

float32x4_t __attribute__ ((noinline))
test_vmulxq_laneq_f32_lane1 (float32x4_t vec1_1, float32x4_t vec1_2)
{
  return vmulxq_laneq_f32 (vec1_1, vec1_2, 1);
}

float32x4_t __attribute__ ((noinline))
test_vmulxq_laneq_f32_lane2 (float32x4_t vec1_1, float32x4_t vec1_2)
{
  return vmulxq_laneq_f32 (vec1_1, vec1_2, 2);
}

float32x4_t __attribute__ ((noinline))
test_vmulxq_laneq_f32_lane3 (float32x4_t vec1_1, float32x4_t vec1_2)
{
  return vmulxq_laneq_f32 (vec1_1, vec1_2, 3);
}

#define PASS_ARRAY(...) {__VA_ARGS__}

#define SETUP_VEC(V1_D, V2_D, EXP0, EXP1, EXP2, EXP3, I)		\
  void set_and_test_case##I ()						\
  {									\
    int i;								\
    float32_t vec1_data[] = V1_D;					\
    float32x4_t vec1 = vld1q_f32 (vec1_data);				\
    float32_t vec2_data[] =  V2_D;					\
    float32x4_t vec2 = vld1q_f32 (vec2_data);				\
									\
    float32_t expected_lane0[] = EXP0;					\
    float32_t expected_lane1[] = EXP1;					\
    float32_t expected_lane2[] = EXP2;					\
    float32_t expected_lane3[] = EXP3;					\
									\
    float32x4_t actual_lane0_v =					\
      test_vmulxq_laneq_f32_lane0 (vec1, vec2);				\
    float32_t actual_lane0[4];						\
    vst1q_f32 (actual_lane0, actual_lane0_v);				\
    for (i = 0; i < 4; ++i)						\
      if (actual_lane0[i] != expected_lane0[i])				\
	abort ();							\
									\
    float32x4_t actual_lane1_v =					\
      test_vmulxq_laneq_f32_lane1 (vec1, vec2);				\
    float32_t actual_lane1[4];						\
    vst1q_f32 (actual_lane1, actual_lane1_v);				\
    for (i = 0; i < 4; ++i)						\
      if (actual_lane1[i] != expected_lane1[i])				\
	abort ();							\
									\
    float32x4_t actual_lane2_v =					\
      test_vmulxq_laneq_f32_lane2 (vec1, vec2);				\
    float32_t actual_lane2[4];						\
    vst1q_f32 (actual_lane2, actual_lane2_v);				\
    for (i = 0; i < 4; ++i)						\
      if (actual_lane2[i] != expected_lane2[i])				\
	abort ();							\
									\
    float32x4_t actual_lane3_v =					\
      test_vmulxq_laneq_f32_lane3 (vec1, vec2);				\
    float32_t actual_lane3[4];						\
    vst1q_f32 (actual_lane3, actual_lane3_v);				\
    for (i = 0; i < 4; ++i)						\
      if (actual_lane3[i] != expected_lane3[i])				\
	abort ();							\
  }									\

float32_t v1 = 3.14159265359;
float32_t v2 = 1.383894;
float32_t v3 = -2.71828;
float32_t v4 = -3.4891931;

float32_t v5 = 0.0;
float32_t v6 = -0.0;
float32_t v7 = __builtin_huge_valf ();
float32_t v8 = -__builtin_huge_valf ();

float32_t spec = __builtin_huge_valf () * __builtin_huge_valf ();
float32_t spec_n = -__builtin_huge_valf () * __builtin_huge_valf ();

SETUP_VEC (PASS_ARRAY (v1, v2, v3, v4), PASS_ARRAY (v1, v2, v3, v4),
	   PASS_ARRAY (v1*v1, v1*v2, v1*v3, v1*v4),
	   PASS_ARRAY (v1*v2, v2*v2, v2*v3, v2*v4),
	   PASS_ARRAY (v1*v3, v2*v3, v3*v3, v4*v3),
	   PASS_ARRAY (v1*v4, v2*v4, v3*v4, v4*v4), 1)

SETUP_VEC (PASS_ARRAY (v5, v6, v7, v8), PASS_ARRAY (v5, v6, v7, v8),
	   PASS_ARRAY (0.0, -0.0, 2.0, -2.0),
	   PASS_ARRAY (-0.0, 0.0, -2.0, 2.0),
	   PASS_ARRAY (2.0, -2.0, spec, spec_n),
	   PASS_ARRAY (-2.0, 2.0, spec_n, spec), 2)

int
main (void)
{
  set_and_test_case1 ();
  set_and_test_case2 ();
  return 0;
}
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[0\\\]\n" 1 } } */
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[1\\\]\n" 1 } } */
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[2\\\]\n" 1 } } */
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[3\\\]\n" 1 } } */
