/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

#define TEST_LANE(NAME, RET_TYPE, TYPE1, TYPE2, TYPE3)                         \
  RET_TYPE test_##NAME (TYPE1 a, TYPE2 b, TYPE3 c)                             \
  {                                                                            \
    return NAME (a, b, c, VEC_LEN (c) - 1);                                    \
  }

/*
** test_vmls_s8:
** mls	v0\.8b, (v1\.8b, v2\.8b|v2\.8b, v1\.8b)
** ret
*/
TEST_UNIFORM_TERNARY (vmls_s8, int8x8_t)

/*
** test_vmls_u8:
** mls	v0\.8b, (v1\.8b, v2\.8b|v2\.8b, v1\.8b)
** ret
*/
TEST_UNIFORM_TERNARY (vmls_u8, uint8x8_t)

/*
** test_vmlsq_s8:
** mls	v0\.16b, (v1\.16b, v2\.16b|v2\.16b, v1\.16b)
** ret
*/
TEST_UNIFORM_TERNARY (vmlsq_s8, int8x16_t)

/*
** test_vmlsq_u8:
** mls	v0\.16b, (v1\.16b, v2\.16b|v2\.16b, v1\.16b)
** ret
*/
TEST_UNIFORM_TERNARY (vmlsq_u8, uint8x16_t)

/*
** test_vmls_s16:
** mls	v0\.4h, (v1\.4h, v2\.4h|v2\.4h, v1\.4h)
** ret
*/
TEST_UNIFORM_TERNARY (vmls_s16, int16x4_t)

/*
** test_vmls_u16:
** mls	v0\.4h, (v1\.4h, v2\.4h|v2\.4h, v1\.4h)
** ret
*/
TEST_UNIFORM_TERNARY (vmls_u16, uint16x4_t)

/*
** test_vmlsq_s16:
** mls	v0\.8h, (v1\.8h, v2\.8h|v2\.8h, v1\.8h)
** ret
*/
TEST_UNIFORM_TERNARY (vmlsq_s16, int16x8_t)

/*
** test_vmlsq_u16:
** mls	v0\.8h, (v1\.8h, v2\.8h|v2\.8h, v1\.8h)
** ret
*/
TEST_UNIFORM_TERNARY (vmlsq_u16, uint16x8_t)

/*
** test_vmls_s32:
** mls	v0\.2s, (v1\.2s, v2\.2s|v2\.2s, v1\.2s)
** ret
*/
TEST_UNIFORM_TERNARY (vmls_s32, int32x2_t)

/*
** test_vmls_u32:
** mls	v0\.2s, (v1\.2s, v2\.2s|v2\.2s, v1\.2s)
** ret
*/
TEST_UNIFORM_TERNARY (vmls_u32, uint32x2_t)

/*
** test_vmlsq_s32:
** mls	v0\.4s, (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
** ret
*/
TEST_UNIFORM_TERNARY (vmlsq_s32, int32x4_t)

/*
** test_vmlsq_u32:
** mls	v0\.4s, (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
** ret
*/
TEST_UNIFORM_TERNARY (vmlsq_u32, uint32x4_t)

/*
** test_vmls_f32:
** fmul	(v[0-9]+\.2s), (v1\.2s, v2\.2s|v2\.2s, v1\.2s)
** fsub	v0\.2s, (\1, v0\.2s|v0\.2s, \1)
** ret
*/
TEST_UNIFORM_TERNARY (vmls_f32, float32x2_t)

/*
** test_vmlsq_f32:
** fmul	(v[0-9]+\.4s), (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
** fsub	v0\.4s, (\1, v0\.4s|v0\.4s, \1)
** ret
*/
TEST_UNIFORM_TERNARY (vmlsq_f32, float32x4_t)

/*
** test_vmls_f64:
** fmul	(d[0-9]+), (d1, d2|d2, d1)
** fsub	d0, (\1, d0|d0, \1)
** ret
*/
TEST_UNIFORM_TERNARY (vmls_f64, float64x1_t)

/*
** test_vmlsq_f64:
** fmul	(v[0-9]+\.2d), (v1\.2d, v2\.2d|v2\.2d, v1\.2d)
** fsub	v0\.2d, (\1, v0\.2d|v0\.2d, \1)
** ret
*/
TEST_UNIFORM_TERNARY (vmlsq_f64, float64x2_t)

/*
** test_vmls_n_s16:
** dup	v([0-9]+)\.4h, w0
** mls	v0\.4h, v1\.4h, v\1\.h\[0\]
** ret
*/
TEST_TERNARY (vmls_n_s16, int16x4_t, int16x4_t, int16x4_t, int16_t)

/*
** test_vmls_n_u16:
** dup	v([0-9]+)\.4h, w0
** mls	v0\.4h, v1\.4h, v\1\.h\[0\]
** ret
*/
TEST_TERNARY (vmls_n_u16, uint16x4_t, uint16x4_t, uint16x4_t, uint16_t)

/*
** test_vmlsq_n_s16:
** dup	v([0-9]+)\.4h, w0
** mls	v0\.8h, v1\.8h, v\1\.h\[0\]
** ret
*/
TEST_TERNARY (vmlsq_n_s16, int16x8_t, int16x8_t, int16x8_t, int16_t)

/*
** test_vmlsq_n_u16:
** dup	v([0-9]+)\.4h, w0
** mls	v0\.8h, v1\.8h, v\1\.h\[0\]
** ret
*/
TEST_TERNARY (vmlsq_n_u16, uint16x8_t, uint16x8_t, uint16x8_t, uint16_t)

/*
** test_vmls_n_s32:
** fmov	s([0-9]+), w0
** mls	v0\.2s, v1\.2s, v\1\.s\[0\]
** ret
*/
TEST_TERNARY (vmls_n_s32, int32x2_t, int32x2_t, int32x2_t, int32_t)

/*
** test_vmls_n_u32:
** fmov	s([0-9]+), w0
** mls	v0\.2s, v1\.2s, v\1\.s\[0\]
** ret
*/
TEST_TERNARY (vmls_n_u32, uint32x2_t, uint32x2_t, uint32x2_t, uint32_t)

/*
** test_vmlsq_n_s32:
** fmov	s([0-9]+), w0
** mls	v0\.4s, v1\.4s, v\1\.s\[0\]
** ret
*/
TEST_TERNARY (vmlsq_n_s32, int32x4_t, int32x4_t, int32x4_t, int32_t)

/*
** test_vmlsq_n_u32:
** fmov	s([0-9]+), w0
** mls	v0\.4s, v1\.4s, v\1\.s\[0\]
** ret
*/
TEST_TERNARY (vmlsq_n_u32, uint32x4_t, uint32x4_t, uint32x4_t, uint32_t)

/*
** test_vmls_n_f32:
** fmul	(v[0-9]+\.2s), v1\.2s, v2\.s\[0\]
** fsub	v0\.2s, (\1, v0\.2s|v0\.2s, \1)
** ret
*/
TEST_TERNARY (vmls_n_f32, float32x2_t, float32x2_t, float32x2_t, float32_t)

/*
** test_vmlsq_n_f32:
** fmul	(v[0-9]+\.4s), v1\.4s, v2\.s\[0\]
** fsub	v0\.4s, (\1, v0\.4s|v0\.4s, \1)
** ret
*/
TEST_TERNARY (vmlsq_n_f32, float32x4_t, float32x4_t, float32x4_t, float32_t)

/*
** test_vmls_lane_s16:
** mls	v0\.4h, v1\.4h, v2\.h\[3\]
** ret
*/
TEST_LANE (vmls_lane_s16, int16x4_t, int16x4_t, int16x4_t, int16x4_t)

/*
** test_vmls_lane_u16:
** mls	v0\.4h, v1\.4h, v2\.h\[3\]
** ret
*/
TEST_LANE (vmls_lane_u16, uint16x4_t, uint16x4_t, uint16x4_t, uint16x4_t)

/*
** test_vmls_lane_s32:
** mls	v0\.2s, v1\.2s, v2\.s\[1\]
** ret
*/
TEST_LANE (vmls_lane_s32, int32x2_t, int32x2_t, int32x2_t, int32x2_t)

/*
** test_vmls_lane_u32:
** mls	v0\.2s, v1\.2s, v2\.s\[1\]
** ret
*/
TEST_LANE (vmls_lane_u32, uint32x2_t, uint32x2_t, uint32x2_t, uint32x2_t)

/*
** test_vmls_lane_f32:
** fmul	(v[0-9]+\.2s), v1\.2s, v2\.s\[1\]
** fsub	v0\.2s, (\1, v0\.2s|v0\.2s, \1)
** ret
*/
TEST_LANE (vmls_lane_f32, float32x2_t, float32x2_t, float32x2_t, float32x2_t)

/*
** test_vmls_laneq_s16:
** mls	v0\.4h, v1\.4h, v2\.h\[7\]
** ret
*/
TEST_LANE (vmls_laneq_s16, int16x4_t, int16x4_t, int16x4_t, int16x8_t)

/*
** test_vmls_laneq_u16:
** mls	v0\.4h, v1\.4h, v2\.h\[7\]
** ret
*/
TEST_LANE (vmls_laneq_u16, uint16x4_t, uint16x4_t, uint16x4_t, uint16x8_t)

/*
** test_vmls_laneq_s32:
** mls	v0\.2s, v1\.2s, v2\.s\[3\]
** ret
*/
TEST_LANE (vmls_laneq_s32, int32x2_t, int32x2_t, int32x2_t, int32x4_t)

/*
** test_vmls_laneq_u32:
** mls	v0\.2s, v1\.2s, v2\.s\[3\]
** ret
*/
TEST_LANE (vmls_laneq_u32, uint32x2_t, uint32x2_t, uint32x2_t, uint32x4_t)

/*
** test_vmls_laneq_f32:
** fmul	(v[0-9]+\.2s), v1\.2s, v2\.s\[3\]
** fsub	v0\.2s, (\1, v0\.2s|v0\.2s, \1)
** ret
*/
TEST_LANE (vmls_laneq_f32, float32x2_t, float32x2_t, float32x2_t, float32x4_t)

/*
** test_vmlsq_lane_s16:
** mls	v0\.8h, v1\.8h, v2\.h\[3\]
** ret
*/
TEST_LANE (vmlsq_lane_s16, int16x8_t, int16x8_t, int16x8_t, int16x4_t)

/*
** test_vmlsq_lane_u16:
** mls	v0\.8h, v1\.8h, v2\.h\[3\]
** ret
*/
TEST_LANE (vmlsq_lane_u16, uint16x8_t, uint16x8_t, uint16x8_t, uint16x4_t)

/*
** test_vmlsq_lane_s32:
** mls	v0\.4s, v1\.4s, v2\.s\[1\]
** ret
*/
TEST_LANE (vmlsq_lane_s32, int32x4_t, int32x4_t, int32x4_t, int32x2_t)

/*
** test_vmlsq_lane_u32:
** mls	v0\.4s, v1\.4s, v2\.s\[1\]
** ret
*/
TEST_LANE (vmlsq_lane_u32, uint32x4_t, uint32x4_t, uint32x4_t, uint32x2_t)

/*
** test_vmlsq_lane_f32:
** fmul	(v[0-9]+\.4s), v1\.4s, v2\.s\[1\]
** fsub	v0\.4s, (\1, v0\.4s|v0\.4s, \1)
** ret
*/
TEST_LANE (vmlsq_lane_f32, float32x4_t, float32x4_t, float32x4_t, float32x2_t)

/*
** test_vmlsq_laneq_s16:
** mls	v0\.8h, v1\.8h, v2\.h\[7\]
** ret
*/
TEST_LANE (vmlsq_laneq_s16, int16x8_t, int16x8_t, int16x8_t, int16x8_t)

/*
** test_vmlsq_laneq_u16:
** mls	v0\.8h, v1\.8h, v2\.h\[7\]
** ret
*/
TEST_LANE (vmlsq_laneq_u16, uint16x8_t, uint16x8_t, uint16x8_t, uint16x8_t)

/*
** test_vmlsq_laneq_s32:
** mls	v0\.4s, v1\.4s, v2\.s\[3\]
** ret
*/
TEST_LANE (vmlsq_laneq_s32, int32x4_t, int32x4_t, int32x4_t, int32x4_t)

/*
** test_vmlsq_laneq_u32:
** mls	v0\.4s, v1\.4s, v2\.s\[3\]
** ret
*/
TEST_LANE (vmlsq_laneq_u32, uint32x4_t, uint32x4_t, uint32x4_t, uint32x4_t)

/*
** test_vmlsq_laneq_f32:
** fmul	(v[0-9]+\.4s), v1\.4s, v2\.s\[3\]
** fsub	v0\.4s, (\1, v0\.4s|v0\.4s, \1)
** ret
*/
TEST_LANE (vmlsq_laneq_f32, float32x4_t, float32x4_t, float32x4_t, float32x4_t)
