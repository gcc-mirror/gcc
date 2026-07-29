/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vmul_s8:
** mul	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vmul_s8, int8x8_t)

/*
** test_vmulq_s8:
** mul	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vmulq_s8, int8x16_t)

/*
** test_vmul_s16:
** mul	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vmul_s16, int16x4_t)

/*
** test_vmulq_s16:
** mul	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vmulq_s16, int16x8_t)

/*
** test_vmul_s32:
** mul	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vmul_s32, int32x2_t)

/*
** test_vmulq_s32:
** mul	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vmulq_s32, int32x4_t)

/*
** test_vmul_u8:
** mul	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vmul_u8, uint8x8_t)

/*
** test_vmulq_u8:
** mul	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vmulq_u8, uint8x16_t)

/*
** test_vmul_u16:
** mul	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vmul_u16, uint16x4_t)

/*
** test_vmulq_u16:
** mul	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vmulq_u16, uint16x8_t)

/*
** test_vmul_u32:
** mul	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vmul_u32, uint32x2_t)

/*
** test_vmulq_u32:
** mul	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vmulq_u32, uint32x4_t)

/*
** test_vmul_f32:
** fmul	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vmul_f32, float32x2_t)

/*
** test_vmulq_f32:
** fmul	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vmulq_f32, float32x4_t)

/*
** test_vmul_f64:
** fmul	d0, (d0, d1|d1, d0)
** ret
*/
TEST_UNIFORM_BINARY (vmul_f64, float64x1_t)

/*
** test_vmulq_f64:
** fmul	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_UNIFORM_BINARY (vmulq_f64, float64x2_t)

/*
** test_vmul_f16:
** fmul	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vmul_f16, float16x4_t)

/*
** test_vmulq_f16:
** fmul	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vmulq_f16, float16x8_t)

/*
** test_vmul_n_f16:
** fmul	v0\.4h, v0\.4h, v1\.h\[0\]
** ret
*/
TEST_BINARY (vmul_n_f16, float16x4_t, float16x4_t, float16_t)

/*
** test_vmulq_n_f16:
** fmul	v0\.8h, v0\.8h, v1\.h\[0\]
** ret
*/
TEST_BINARY (vmulq_n_f16, float16x8_t, float16x8_t, float16_t)

/*
** test_vmul_n_f32:
** fmul	v0\.2s, v0\.2s, v1\.s\[0\]
** ret
*/
TEST_BINARY (vmul_n_f32, float32x2_t, float32x2_t, float32_t)

/*
** test_vmulq_n_f32:
** fmul	v0\.4s, v0\.4s, v1\.s\[0\]
** ret
*/
TEST_BINARY (vmulq_n_f32, float32x4_t, float32x4_t, float32_t)

/*
** test_vmul_n_f64:
** fmul	d0, (d0, d1|d1, d0)
** ret
*/
TEST_BINARY (vmul_n_f64, float64x1_t, float64x1_t, float64_t)

/*
** test_vmulq_n_f64:
** fmul	v0\.2d, v0\.2d, v1\.d\[0\]
** ret
*/
TEST_BINARY (vmulq_n_f64, float64x2_t, float64x2_t, float64_t)

/*
** test_vmul_n_s16:
** dup	v([0-9]+)\.4h, w0
** mul	v0\.4h, v0\.4h, v\1\.h\[0\]
** ret
*/
TEST_BINARY (vmul_n_s16, int16x4_t, int16x4_t, int16_t)

/*
** test_vmulq_n_s16:
** dup	v([0-9]+)\.4h, w0
** mul	v0\.8h, v0\.8h, v\1\.h\[0\]
** ret
*/
TEST_BINARY (vmulq_n_s16, int16x8_t, int16x8_t, int16_t)

/*
** test_vmul_n_s32:
** fmov	s([0-9]+), w0
** mul	v0\.2s, v0\.2s, v\1\.s\[0\]
** ret
*/
TEST_BINARY (vmul_n_s32, int32x2_t, int32x2_t, int32_t)

/*
** test_vmulq_n_s32:
** fmov	s([0-9]+), w0
** mul	v0\.4s, v0\.4s, v\1\.s\[0\]
** ret
*/
TEST_BINARY (vmulq_n_s32, int32x4_t, int32x4_t, int32_t)

/*
** test_vmul_n_u16:
** dup	v([0-9]+)\.4h, w0
** mul	v0\.4h, v0\.4h, v\1\.h\[0\]
** ret
*/
TEST_BINARY (vmul_n_u16, uint16x4_t, uint16x4_t, uint16_t)

/*
** test_vmulq_n_u16:
** dup	v([0-9]+)\.4h, w0
** mul	v0\.8h, v0\.8h, v\1\.h\[0\]
** ret
*/
TEST_BINARY (vmulq_n_u16, uint16x8_t, uint16x8_t, uint16_t)

/*
** test_vmul_n_u32:
** fmov	s([0-9]+), w0
** mul	v0\.2s, v0\.2s, v\1\.s\[0\]
** ret
*/
TEST_BINARY (vmul_n_u32, uint32x2_t, uint32x2_t, uint32_t)

/*
** test_vmulq_n_u32:
** fmov	s([0-9]+), w0
** mul	v0\.4s, v0\.4s, v\1\.s\[0\]
** ret
*/
TEST_BINARY (vmulq_n_u32, uint32x4_t, uint32x4_t, uint32_t)

/*
** test_vmul_lane_f16:
** fmul	v0\.4h, v0\.4h, v1\.h\[3\]
** ret
*/
TEST_BINARY_LANE (vmul_lane_f16, float16x4_t, float16x4_t, float16x4_t)

/*
** test_vmul_laneq_f16:
** fmul	v0\.4h, v0\.4h, v1\.h\[7\]
** ret
*/
TEST_BINARY_LANE (vmul_laneq_f16, float16x4_t, float16x4_t, float16x8_t)

/*
** test_vmulq_lane_f16:
** fmul	v0\.8h, v0\.8h, v1\.h\[3\]
** ret
*/
TEST_BINARY_LANE (vmulq_lane_f16, float16x8_t, float16x8_t, float16x4_t)

/*
** test_vmulq_laneq_f16:
** fmul	v0\.8h, v0\.8h, v1\.h\[7\]
** ret
*/
TEST_BINARY_LANE (vmulq_laneq_f16, float16x8_t, float16x8_t, float16x8_t)

/*
** test_vmul_lane_f32:
** fmul	v0\.2s, v0\.2s, v1\.s\[1\]
** ret
*/
TEST_BINARY_LANE (vmul_lane_f32, float32x2_t, float32x2_t, float32x2_t)

/*
** test_vmul_laneq_f32:
** fmul	v0\.2s, v0\.2s, v1\.s\[3\]
** ret
*/
TEST_BINARY_LANE (vmul_laneq_f32, float32x2_t, float32x2_t, float32x4_t)

/*
** test_vmulq_lane_f32:
** fmul	v0\.4s, v0\.4s, v1\.s\[1\]
** ret
*/
TEST_BINARY_LANE (vmulq_lane_f32, float32x4_t, float32x4_t, float32x2_t)

/*
** test_vmulq_laneq_f32:
** fmul	v0\.4s, v0\.4s, v1\.s\[3\]
** ret
*/
TEST_BINARY_LANE (vmulq_laneq_f32, float32x4_t, float32x4_t, float32x4_t)

/*
** test_vmul_lane_f64:
** fmul	d0, (d0, d1|d1, d0)
** ret
*/
TEST_BINARY_LANE (vmul_lane_f64, float64x1_t, float64x1_t, float64x1_t)

/*
** test_vmul_laneq_f64:
** fmul	v0\.2d, v0\.2d, v1\.d\[1\]
** ret
*/
TEST_BINARY_LANE (vmul_laneq_f64, float64x1_t, float64x1_t, float64x2_t)

/*
** test_vmulq_lane_f64:
** fmul	v0\.2d, v0\.2d, v1\.d\[0\]
** ret
*/
TEST_BINARY_LANE (vmulq_lane_f64, float64x2_t, float64x2_t, float64x1_t)

/*
** test_vmulq_laneq_f64:
** fmul	v0\.2d, v0\.2d, v1\.d\[1\]
** ret
*/
TEST_BINARY_LANE (vmulq_laneq_f64, float64x2_t, float64x2_t, float64x2_t)

/*
** test_vmul_lane_s16:
** mul	v0\.4h, v0\.4h, v1\.h\[3\]
** ret
*/
TEST_BINARY_LANE (vmul_lane_s16, int16x4_t, int16x4_t, int16x4_t)

/*
** test_vmul_laneq_s16:
** mul	v0\.4h, v0\.4h, v1\.h\[7\]
** ret
*/
TEST_BINARY_LANE (vmul_laneq_s16, int16x4_t, int16x4_t, int16x8_t)

/*
** test_vmulq_lane_s16:
** mul	v0\.8h, v0\.8h, v1\.h\[3\]
** ret
*/
TEST_BINARY_LANE (vmulq_lane_s16, int16x8_t, int16x8_t, int16x4_t)

/*
** test_vmulq_laneq_s16:
** mul	v0\.8h, v0\.8h, v1\.h\[7\]
** ret
*/
TEST_BINARY_LANE (vmulq_laneq_s16, int16x8_t, int16x8_t, int16x8_t)

/*
** test_vmul_lane_s32:
** mul	v0\.2s, v0\.2s, v1\.s\[1\]
** ret
*/
TEST_BINARY_LANE (vmul_lane_s32, int32x2_t, int32x2_t, int32x2_t)

/*
** test_vmul_laneq_s32:
** mul	v0\.2s, v0\.2s, v1\.s\[3\]
** ret
*/
TEST_BINARY_LANE (vmul_laneq_s32, int32x2_t, int32x2_t, int32x4_t)

/*
** test_vmulq_lane_s32:
** mul	v0\.4s, v0\.4s, v1\.s\[1\]
** ret
*/
TEST_BINARY_LANE (vmulq_lane_s32, int32x4_t, int32x4_t, int32x2_t)

/*
** test_vmulq_laneq_s32:
** mul	v0\.4s, v0\.4s, v1\.s\[3\]
** ret
*/
TEST_BINARY_LANE (vmulq_laneq_s32, int32x4_t, int32x4_t, int32x4_t)

/*
** test_vmul_lane_u16:

** mul	v0\.4h, v0\.4h, v1\.h\[3\]
** ret
*/
TEST_BINARY_LANE (vmul_lane_u16, uint16x4_t, uint16x4_t, uint16x4_t)

/*
** test_vmul_laneq_u16:
** mul	v0\.4h, v0\.4h, v1\.h\[7\]
** ret
*/
TEST_BINARY_LANE (vmul_laneq_u16, uint16x4_t, uint16x4_t, uint16x8_t)

/*
** test_vmulq_lane_u16:
** mul	v0\.8h, v0\.8h, v1\.h\[3\]
** ret
*/
TEST_BINARY_LANE (vmulq_lane_u16, uint16x8_t, uint16x8_t, uint16x4_t)

/*
** test_vmulq_laneq_u16:
** mul	v0\.8h, v0\.8h, v1\.h\[7\]
** ret
*/
TEST_BINARY_LANE (vmulq_laneq_u16, uint16x8_t, uint16x8_t, uint16x8_t)

/*
** test_vmul_lane_u32:
** mul	v0\.2s, v0\.2s, v1\.s\[1\]
** ret
*/
TEST_BINARY_LANE (vmul_lane_u32, uint32x2_t, uint32x2_t, uint32x2_t)

/*
** test_vmul_laneq_u32:
** mul	v0\.2s, v0\.2s, v1\.s\[3\]
** ret
*/
TEST_BINARY_LANE (vmul_laneq_u32, uint32x2_t, uint32x2_t, uint32x4_t)

/*
** test_vmulq_lane_u32:
** mul	v0\.4s, v0\.4s, v1\.s\[1\]
** ret
*/
TEST_BINARY_LANE (vmulq_lane_u32, uint32x4_t, uint32x4_t, uint32x2_t)

/*
** test_vmulq_laneq_u32:
** mul	v0\.4s, v0\.4s, v1\.s\[3\]
** ret
*/
TEST_BINARY_LANE (vmulq_laneq_u32, uint32x4_t, uint32x4_t, uint32x4_t)

/*
** test_vmulh_lane_f16:
** dup	h([0-9]+), v1\.h\[3\]
** fmul	h0, (h0, h\1|h\1, h0)
** ret
*/
TEST_BINARY_LANE (vmulh_lane_f16, float16_t, float16_t, float16x4_t)

/*
** test_vmulh_laneq_f16:
** dup	h([0-9]+), v1\.h\[7\]
** fmul	h0, (h0, h\1|h\1, h0)
** ret
*/
TEST_BINARY_LANE (vmulh_laneq_f16, float16_t, float16_t, float16x8_t)

/*
** test_vmuls_lane_f32:
** dup	s([0-9]+), v1\.s\[1\]
** fmul	s0, (s0, s\1|s\1, s0)
** ret
*/
TEST_BINARY_LANE (vmuls_lane_f32, float32_t, float32_t, float32x2_t)

/*
** test_vmuls_laneq_f32:
** dup	s([0-9]+), v1\.s\[3\]
** fmul	s0, (s0, s\1|s\1, s0)
** ret
*/
TEST_BINARY_LANE (vmuls_laneq_f32, float32_t, float32_t, float32x4_t)

/*
** test_vmuld_lane_f64:
** fmul	d0, (d0, d1|d1, d0)
** ret
*/
TEST_BINARY_LANE (vmuld_lane_f64, float64_t, float64_t, float64x1_t)

/*
** test_vmuld_laneq_f64:
** fmul	v0\.2d, v0\.2d, v1\.d\[1\]
** ret
*/
TEST_BINARY_LANE (vmuld_laneq_f64, float64_t, float64_t, float64x2_t)
