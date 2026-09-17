/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

#define TEST_LANE(NAME, RET_TYPE, TYPE1, TYPE2, TYPE3)                         \
  RET_TYPE test_##NAME (TYPE1 a, TYPE2 b, TYPE3 c)                             \
  {                                                                            \
    return NAME (a, b, c, VEC_LEN (c) - 1);                                    \
  }

/*
** test_vmla_s8:
** mla	v0\.8b, (v1\.8b, v2\.8b|v2\.8b, v1\.8b)
** ret
*/
TEST_UNIFORM_TERNARY (vmla_s8, int8x8_t)

/*
** test_vmla_u8:
** mla	v0\.8b, (v1\.8b, v2\.8b|v2\.8b, v1\.8b)
** ret
*/
TEST_UNIFORM_TERNARY (vmla_u8, uint8x8_t)

/*
** test_vmlaq_s8:
** mla	v0\.16b, (v1\.16b, v2\.16b|v2\.16b, v1\.16b)
** ret
*/
TEST_UNIFORM_TERNARY (vmlaq_s8, int8x16_t)

/*
** test_vmlaq_u8:
** mla	v0\.16b, (v1\.16b, v2\.16b|v2\.16b, v1\.16b)
** ret
*/
TEST_UNIFORM_TERNARY (vmlaq_u8, uint8x16_t)

/*
** test_vmla_s16:
** mla	v0\.4h, (v1\.4h, v2\.4h|v2\.4h, v1\.4h)
** ret
*/
TEST_UNIFORM_TERNARY (vmla_s16, int16x4_t)

/*
** test_vmla_u16:
** mla	v0\.4h, (v1\.4h, v2\.4h|v2\.4h, v1\.4h)
** ret
*/
TEST_UNIFORM_TERNARY (vmla_u16, uint16x4_t)

/*
** test_vmlaq_s16:
** mla	v0\.8h, (v1\.8h, v2\.8h|v2\.8h, v1\.8h)
** ret
*/
TEST_UNIFORM_TERNARY (vmlaq_s16, int16x8_t)

/*
** test_vmlaq_u16:
** mla	v0\.8h, (v1\.8h, v2\.8h|v2\.8h, v1\.8h)
** ret
*/
TEST_UNIFORM_TERNARY (vmlaq_u16, uint16x8_t)

/*
** test_vmla_s32:
** mla	v0\.2s, (v1\.2s, v2\.2s|v2\.2s, v1\.2s)
** ret
*/
TEST_UNIFORM_TERNARY (vmla_s32, int32x2_t)

/*
** test_vmla_u32:
** mla	v0\.2s, (v1\.2s, v2\.2s|v2\.2s, v1\.2s)
** ret
*/
TEST_UNIFORM_TERNARY (vmla_u32, uint32x2_t)

/*
** test_vmlaq_s32:
** mla	v0\.4s, (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
** ret
*/
TEST_UNIFORM_TERNARY (vmlaq_s32, int32x4_t)

/*
** test_vmlaq_u32:
** mla	v0\.4s, (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
** ret
*/
TEST_UNIFORM_TERNARY (vmlaq_u32, uint32x4_t)

/*
** test_vmla_f32:
** fmul	(v[0-9]+\.2s), (v1\.2s, v2\.2s|v2\.2s, v1\.2s)
** fadd	v0\.2s, (\1, v0\.2s|v0\.2s, \1)
** ret
*/
TEST_UNIFORM_TERNARY (vmla_f32, float32x2_t)

/*
** test_vmlaq_f32:
** fmul	(v[0-9]+\.4s), (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
** fadd	v0\.4s, (\1, v0\.4s|v0\.4s, \1)
** ret
*/
TEST_UNIFORM_TERNARY (vmlaq_f32, float32x4_t)

/*
** test_vmla_f64:
** fmul	(d[0-9]+), (d1, d2|d2, d1)
** fadd	d0, (\1, d0|d0, \1)
** ret
*/
TEST_UNIFORM_TERNARY (vmla_f64, float64x1_t)

/*
** test_vmlaq_f64:
** fmul	(v[0-9]+\.2d), (v1\.2d, v2\.2d|v2\.2d, v1\.2d)
** fadd	v0\.2d, (\1, v0\.2d|v0\.2d, \1)
** ret
*/
TEST_UNIFORM_TERNARY (vmlaq_f64, float64x2_t)

/*
** test_vmla_n_s16:
** dup	v([0-9]+)\.4h, w0
** mla	v0\.4h, v1\.4h, v\1\.h\[0\]
** ret
*/
TEST_TERNARY (vmla_n_s16, int16x4_t, int16x4_t, int16x4_t, int16_t)

/*
** test_vmla_n_u16:
** dup	v([0-9]+)\.4h, w0
** mla	v0\.4h, v1\.4h, v\1\.h\[0\]
** ret
*/
TEST_TERNARY (vmla_n_u16, uint16x4_t, uint16x4_t, uint16x4_t, uint16_t)

/*
** test_vmlaq_n_s16:
** dup	v([0-9]+)\.4h, w0
** mla	v0\.8h, v1\.8h, v\1\.h\[0\]
** ret
*/
TEST_TERNARY (vmlaq_n_s16, int16x8_t, int16x8_t, int16x8_t, int16_t)

/*
** test_vmlaq_n_u16:
** dup	v([0-9]+)\.4h, w0
** mla	v0\.8h, v1\.8h, v\1\.h\[0\]
** ret
*/
TEST_TERNARY (vmlaq_n_u16, uint16x8_t, uint16x8_t, uint16x8_t, uint16_t)

/*
** test_vmla_n_s32:
** fmov	s([0-9]+), w0
** mla	v0\.2s, v1\.2s, v\1\.s\[0\]
** ret
*/
TEST_TERNARY (vmla_n_s32, int32x2_t, int32x2_t, int32x2_t, int32_t)

/*
** test_vmla_n_u32:
** fmov	s([0-9]+), w0
** mla	v0\.2s, v1\.2s, v\1\.s\[0\]
** ret
*/
TEST_TERNARY (vmla_n_u32, uint32x2_t, uint32x2_t, uint32x2_t, uint32_t)

/*
** test_vmlaq_n_s32:
** fmov	s([0-9]+), w0
** mla	v0\.4s, v1\.4s, v\1\.s\[0\]
** ret
*/
TEST_TERNARY (vmlaq_n_s32, int32x4_t, int32x4_t, int32x4_t, int32_t)

/*
** test_vmlaq_n_u32:
** fmov	s([0-9]+), w0
** mla	v0\.4s, v1\.4s, v\1\.s\[0\]
** ret
*/
TEST_TERNARY (vmlaq_n_u32, uint32x4_t, uint32x4_t, uint32x4_t, uint32_t)

/*
** test_vmla_n_f32:
** fmul	(v[0-9]+\.2s), v1\.2s, v2\.s\[0\]
** fadd	v0\.2s, (\1, v0\.2s|v0\.2s, \1)
** ret
*/
TEST_TERNARY (vmla_n_f32, float32x2_t, float32x2_t, float32x2_t, float32_t)

/*
** test_vmlaq_n_f32:
** fmul	(v[0-9]+\.4s), v1\.4s, v2\.s\[0\]
** fadd	v0\.4s, (\1, v0\.4s|v0\.4s, \1)
** ret
*/
TEST_TERNARY (vmlaq_n_f32, float32x4_t, float32x4_t, float32x4_t, float32_t)

/*
** test_vmla_lane_s16:
** mla	v0\.4h, v1\.4h, v2\.h\[3\]
** ret
*/
TEST_LANE (vmla_lane_s16, int16x4_t, int16x4_t, int16x4_t, int16x4_t)

/*
** test_vmla_lane_u16:
** mla	v0\.4h, v1\.4h, v2\.h\[3\]
** ret
*/
TEST_LANE (vmla_lane_u16, uint16x4_t, uint16x4_t, uint16x4_t, uint16x4_t)

/*
** test_vmla_lane_s32:
** mla	v0\.2s, v1\.2s, v2\.s\[1\]
** ret
*/
TEST_LANE (vmla_lane_s32, int32x2_t, int32x2_t, int32x2_t, int32x2_t)

/*
** test_vmla_lane_u32:
** mla	v0\.2s, v1\.2s, v2\.s\[1\]
** ret
*/
TEST_LANE (vmla_lane_u32, uint32x2_t, uint32x2_t, uint32x2_t, uint32x2_t)

/*
** test_vmla_lane_f32:
** fmul	(v[0-9]+\.2s), v1\.2s, v2\.s\[1\]
** fadd	v0\.2s, (\1, v0\.2s|v0\.2s, \1)
** ret
*/
TEST_LANE (vmla_lane_f32, float32x2_t, float32x2_t, float32x2_t, float32x2_t)

/*
** test_vmla_laneq_s16:
** mla	v0\.4h, v1\.4h, v2\.h\[7\]
** ret
*/
TEST_LANE (vmla_laneq_s16, int16x4_t, int16x4_t, int16x4_t, int16x8_t)

/*
** test_vmla_laneq_u16:
** mla	v0\.4h, v1\.4h, v2\.h\[7\]
** ret
*/
TEST_LANE (vmla_laneq_u16, uint16x4_t, uint16x4_t, uint16x4_t, uint16x8_t)

/*
** test_vmla_laneq_s32:
** mla	v0\.2s, v1\.2s, v2\.s\[3\]
** ret
*/
TEST_LANE (vmla_laneq_s32, int32x2_t, int32x2_t, int32x2_t, int32x4_t)

/*
** test_vmla_laneq_u32:
** mla	v0\.2s, v1\.2s, v2\.s\[3\]
** ret
*/
TEST_LANE (vmla_laneq_u32, uint32x2_t, uint32x2_t, uint32x2_t, uint32x4_t)

/*
** test_vmla_laneq_f32:
** fmul	(v[0-9]+\.2s), v1\.2s, v2\.s\[3\]
** fadd	v0\.2s, (\1, v0\.2s|v0\.2s, \1)
** ret
*/
TEST_LANE (vmla_laneq_f32, float32x2_t, float32x2_t, float32x2_t, float32x4_t)

/*
** test_vmlaq_lane_s16:
** mla	v0\.8h, v1\.8h, v2\.h\[3\]
** ret
*/
TEST_LANE (vmlaq_lane_s16, int16x8_t, int16x8_t, int16x8_t, int16x4_t)

/*
** test_vmlaq_lane_u16:
** mla	v0\.8h, v1\.8h, v2\.h\[3\]
** ret
*/
TEST_LANE (vmlaq_lane_u16, uint16x8_t, uint16x8_t, uint16x8_t, uint16x4_t)

/*
** test_vmlaq_lane_s32:
** mla	v0\.4s, v1\.4s, v2\.s\[1\]
** ret
*/
TEST_LANE (vmlaq_lane_s32, int32x4_t, int32x4_t, int32x4_t, int32x2_t)

/*
** test_vmlaq_lane_u32:
** mla	v0\.4s, v1\.4s, v2\.s\[1\]
** ret
*/
TEST_LANE (vmlaq_lane_u32, uint32x4_t, uint32x4_t, uint32x4_t, uint32x2_t)

/*
** test_vmlaq_lane_f32:
** fmul	(v[0-9]+\.4s), v1\.4s, v2\.s\[1\]
** fadd	v0\.4s, (\1, v0\.4s|v0\.4s, \1)
** ret
*/
TEST_LANE (vmlaq_lane_f32, float32x4_t, float32x4_t, float32x4_t, float32x2_t)

/*
** test_vmlaq_laneq_s16:
** mla	v0\.8h, v1\.8h, v2\.h\[7\]
** ret
*/
TEST_LANE (vmlaq_laneq_s16, int16x8_t, int16x8_t, int16x8_t, int16x8_t)

/*
** test_vmlaq_laneq_u16:
** mla	v0\.8h, v1\.8h, v2\.h\[7\]
** ret
*/
TEST_LANE (vmlaq_laneq_u16, uint16x8_t, uint16x8_t, uint16x8_t, uint16x8_t)

/*
** test_vmlaq_laneq_s32:
** mla	v0\.4s, v1\.4s, v2\.s\[3\]
** ret
*/
TEST_LANE (vmlaq_laneq_s32, int32x4_t, int32x4_t, int32x4_t, int32x4_t)

/*
** test_vmlaq_laneq_u32:
** mla	v0\.4s, v1\.4s, v2\.s\[3\]
** ret
*/
TEST_LANE (vmlaq_laneq_u32, uint32x4_t, uint32x4_t, uint32x4_t, uint32x4_t)

/*
** test_vmlaq_laneq_f32:
** fmul	(v[0-9]+\.4s), v1\.4s, v2\.s\[3\]
** fadd	v0\.4s, (\1, v0\.4s|v0\.4s, \1)
** ret
*/
TEST_LANE (vmlaq_laneq_f32, float32x4_t, float32x4_t, float32x4_t, float32x4_t)
