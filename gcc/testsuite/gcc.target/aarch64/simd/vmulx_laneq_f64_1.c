/* Test the vmulx_laneq_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

float64x1_t __attribute__ ((noinline))
test_vmulx_laneq_f64_lane0 (float64x1_t vec1_1, float64x2_t vec1_2)
{
  return vmulx_laneq_f64 (vec1_1, vec1_2, 0);
}

float64x1_t __attribute__ ((noinline))
test_vmulx_laneq_f64_lane1 (float64x1_t vec1_1, float64x2_t vec1_2)
{
  return vmulx_laneq_f64 (vec1_1, vec1_2, 1);
}
#define PASS_ARRAY(...) {__VA_ARGS__}

#define SETUP_VEC(V1_D, V2_D, EXP1, EXP2, I)				\
  void set_and_test_case##I ()						\
  {									\
    float64_t vec1_data[] = V1_D;					\
    float64x1_t vec1 = vld1_f64 (vec1_data);				\
    float64_t vec2_data[] =  V2_D;					\
    float64x2_t vec2 = vld1q_f64 (vec2_data);				\
    float64_t expected_lane0[] = EXP1;					\
    float64_t expected_lane1[] = EXP2;					\
									\
    float64x1_t actual_lane0_v =					\
      test_vmulx_laneq_f64_lane0 (vec1, vec2);				\
    float64_t actual_lane0[1];						\
    vst1_f64 (actual_lane0, actual_lane0_v);				\
    if (actual_lane0[0] != expected_lane0[0])				\
      abort ();								\
									\
    float64x1_t actual_lane1_v =					\
      test_vmulx_laneq_f64_lane1 (vec1, vec2);				\
    float64_t actual_lane1[1];						\
    vst1_f64 (actual_lane1, actual_lane1_v);				\
    if (actual_lane1[0] != expected_lane1[0])				\
      abort ();								\
  }									\

float64_t v1 = 3.14159265359;
float64_t v2 = 1.383894;
float64_t v3 = -2.71828;

float64_t v4 = 0.0;
float64_t v5 = __builtin_huge_val ();
float64_t v6 = -__builtin_huge_val ();

float64_t v7 = -0.0;
float64_t v8 = __builtin_huge_val ();
float64_t v9 = -__builtin_huge_val ();

SETUP_VEC (PASS_ARRAY (v1), PASS_ARRAY (v2, v3), PASS_ARRAY (v1*v2),
	   PASS_ARRAY (v1*v3), 1)
SETUP_VEC (PASS_ARRAY (v4), PASS_ARRAY (v5, v6), PASS_ARRAY (2.0),
	   PASS_ARRAY (-2.0), 2)
SETUP_VEC (PASS_ARRAY (v7), PASS_ARRAY (v8, v9), PASS_ARRAY (-2.0),
	   PASS_ARRAY (2.0), 3)

int
main (void)
{
  set_and_test_case1 ();
  set_and_test_case2 ();
  set_and_test_case3 ();
  return 0;
}
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]\n" 1 } } */
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[dD\]\\\[1\\\]\n" 1 } } */
