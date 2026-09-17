/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

#define TEST_LANE(NAME, RET_TYPE, TYPE1, TYPE2, TYPE3)                         \
  RET_TYPE test_##NAME (TYPE1 a, TYPE2 b, TYPE3 c)                             \
  {                                                                            \
    return NAME (a, b, c, VEC_LEN (c) - 1);                                    \
  }

/*
** test_vfma_f16:
** fmla	v0\.4h, (v1\.4h, v2\.4h|v2\.4h, v1\.4h)
** ret
*/
TEST_UNIFORM_TERNARY (vfma_f16, float16x4_t)

/*
** test_vfmaq_f16:
** fmla	v0\.8h, (v1\.8h, v2\.8h|v2\.8h, v1\.8h)
** ret
*/
TEST_UNIFORM_TERNARY (vfmaq_f16, float16x8_t)

/*
** test_vfma_f32:
** fmla	v0\.2s, (v1\.2s, v2\.2s|v2\.2s, v1\.2s)
** ret
*/
TEST_UNIFORM_TERNARY (vfma_f32, float32x2_t)

/*
** test_vfmaq_f32:
** fmla	v0\.4s, (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
** ret
*/
TEST_UNIFORM_TERNARY (vfmaq_f32, float32x4_t)

/*
** test_vfma_f64:
** fmadd	d0, (d1, d2|d2, d1), d0
** ret
*/
TEST_UNIFORM_TERNARY (vfma_f64, float64x1_t)

/*
** test_vfmaq_f64:
** fmla	v0\.2d, (v1\.2d, v2\.2d|v2\.2d, v1\.2d)
** ret
*/
TEST_UNIFORM_TERNARY (vfmaq_f64, float64x2_t)

/*
** test_vfma_n_f16:
** fmla	v0\.4h, v1\.4h, v2\.h\[0\]
** ret
*/
TEST_TERNARY (vfma_n_f16, float16x4_t, float16x4_t, float16x4_t, float16_t)

/*
** test_vfmaq_n_f16:
** fmla	v0\.8h, v1\.8h, v2\.h\[0\]
** ret
*/
TEST_TERNARY (vfmaq_n_f16, float16x8_t, float16x8_t, float16x8_t, float16_t)

/*
** test_vfma_n_f32:
** fmla	v0\.2s, v1\.2s, v2\.s\[0\]
** ret
*/
TEST_TERNARY (vfma_n_f32, float32x2_t, float32x2_t, float32x2_t, float32_t)

/*
** test_vfmaq_n_f32:
** fmla	v0\.4s, v1\.4s, v2\.s\[0\]
** ret
*/
TEST_TERNARY (vfmaq_n_f32, float32x4_t, float32x4_t, float32x4_t, float32_t)

/*
** test_vfma_n_f64:
** fmadd	d0, (d1, d2|d2, d1), d0
** ret
*/
TEST_TERNARY (vfma_n_f64, float64x1_t, float64x1_t, float64x1_t, float64_t)

/*
** test_vfmaq_n_f64:
** fmla	v0\.2d, v1\.2d, v2\.d\[0\]
** ret
*/
TEST_TERNARY (vfmaq_n_f64, float64x2_t, float64x2_t, float64x2_t, float64_t)

/*
** test_vfma_lane_f16:
** fmla	v0\.4h, v1\.4h, v2\.h\[3\]
** ret
*/
TEST_LANE (vfma_lane_f16, float16x4_t, float16x4_t, float16x4_t, float16x4_t)

/*
** test_vfma_lane_f32:
** fmla	v0\.2s, v1\.2s, v2\.s\[1\]
** ret
*/
TEST_LANE (vfma_lane_f32, float32x2_t, float32x2_t, float32x2_t, float32x2_t)

/*
** test_vfma_lane_f64:
** fmadd	d0, (d1, d2|d2, d1), d0
** ret
*/
TEST_LANE (vfma_lane_f64, float64x1_t, float64x1_t, float64x1_t, float64x1_t)

/*
** test_vfma_laneq_f16:
** fmla	v0\.4h, v1\.4h, v2\.h\[7\]
** ret
*/
TEST_LANE (vfma_laneq_f16, float16x4_t, float16x4_t, float16x4_t, float16x8_t)

/*
** test_vfma_laneq_f32:
** fmla	v0\.2s, v1\.2s, v2\.s\[3\]
** ret
*/
TEST_LANE (vfma_laneq_f32, float32x2_t, float32x2_t, float32x2_t, float32x4_t)

/*
** test_vfma_laneq_f64:
** fmla	d0, d1, v2\.d\[1\]
** ret
*/
TEST_LANE (vfma_laneq_f64, float64x1_t, float64x1_t, float64x1_t, float64x2_t)

/*
** test_vfmaq_lane_f16:
** fmla	v0\.8h, v1\.8h, v2\.h\[3\]
** ret
*/
TEST_LANE (vfmaq_lane_f16, float16x8_t, float16x8_t, float16x8_t, float16x4_t)

/*
** test_vfmaq_lane_f32:
** fmla	v0\.4s, v1\.4s, v2\.s\[1\]
** ret
*/
TEST_LANE (vfmaq_lane_f32, float32x4_t, float32x4_t, float32x4_t, float32x2_t)

/*
** test_vfmaq_lane_f64:
** fmla	v0\.2d, v1\.2d, v2\.d\[0\]
** ret
*/
TEST_LANE (vfmaq_lane_f64, float64x2_t, float64x2_t, float64x2_t, float64x1_t)

/*
** test_vfmaq_laneq_f16:
** fmla	v0\.8h, v1\.8h, v2\.h\[7\]
** ret
*/
TEST_LANE (vfmaq_laneq_f16, float16x8_t, float16x8_t, float16x8_t, float16x8_t)

/*
** test_vfmaq_laneq_f32:
** fmla	v0\.4s, v1\.4s, v2\.s\[3\]
** ret
*/
TEST_LANE (vfmaq_laneq_f32, float32x4_t, float32x4_t, float32x4_t, float32x4_t)

/*
** test_vfmaq_laneq_f64:
** fmla	v0\.2d, v1\.2d, v2\.d\[1\]
** ret
*/
TEST_LANE (vfmaq_laneq_f64, float64x2_t, float64x2_t, float64x2_t, float64x2_t)

/*
** test_vfmah_lane_f16:
** fmla	h0, h1, v2\.h\[3\]
** ret
*/
TEST_LANE (vfmah_lane_f16, float16_t, float16_t, float16_t, float16x4_t)

/*
** test_vfmah_laneq_f16:
** fmla	h0, h1, v2\.h\[7\]
** ret
*/
TEST_LANE (vfmah_laneq_f16, float16_t, float16_t, float16_t, float16x8_t)

/*
** test_vfmas_lane_f32:
** fmla	s0, s1, v2\.s\[1\]
** ret
*/
TEST_LANE (vfmas_lane_f32, float32_t, float32_t, float32_t, float32x2_t)

/*
** test_vfmas_laneq_f32:
** fmla	s0, s1, v2\.s\[3\]
** ret
*/
TEST_LANE (vfmas_laneq_f32, float32_t, float32_t, float32_t, float32x4_t)

/*
** test_vfmad_lane_f64:
** fmadd	d0, (d1, d2|d2, d1), d0
** ret
*/
TEST_LANE (vfmad_lane_f64, float64_t, float64_t, float64_t, float64x1_t)

/*
** test_vfmad_laneq_f64:
** fmla	d0, d1, v2\.d\[1\]
** ret
*/
TEST_LANE (vfmad_laneq_f64, float64_t, float64_t, float64_t, float64x2_t)
