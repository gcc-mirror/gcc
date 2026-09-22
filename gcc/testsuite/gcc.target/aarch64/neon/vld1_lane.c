/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vld1_lane_bf16:
** ld1	{v0\.h}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_bf16, bfloat16_t *, bfloat16x4_t)

/*
** test_vld1_lane_f16:
** ld1	{v0\.h}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_f16, float16_t *, float16x4_t)

/*
** test_vld1_lane_f32:
** ld1	{v0\.s}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_f32, float32_t *, float32x2_t)

/*
** test_vld1_lane_f64:
** ldr	d0, \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_f64, float64_t *, float64x1_t)

/*
** test_vld1_lane_mf8:
** ld1	{v0\.b}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_mf8, mfloat8_t *, mfloat8x8_t)

/*
** test_vld1_lane_s8:
** ld1	{v0\.b}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_s8, int8_t *, int8x8_t)

/*
** test_vld1_lane_s16:
** ld1	{v0\.h}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_s16, int16_t *, int16x4_t)

/*
** test_vld1_lane_s32:
** ld1	{v0\.s}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_s32, int32_t *, int32x2_t)

/*
** test_vld1_lane_s64:
** ldr	d0, \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_s64, int64_t *, int64x1_t)

/*
** test_vld1_lane_u8:
** ld1	{v0\.b}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_u8, uint8_t *, uint8x8_t)

/*
** test_vld1_lane_u16:
** ld1	{v0\.h}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_u16, uint16_t *, uint16x4_t)

/*
** test_vld1_lane_u32:
** ld1	{v0\.s}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_u32, uint32_t *, uint32x2_t)

/*
** test_vld1_lane_u64:
** ldr	d0, \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_u64, uint64_t *, uint64x1_t)

/*
** test_vld1_lane_p8:
** ld1	{v0\.b}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_p8, poly8_t *, poly8x8_t)

/*
** test_vld1_lane_p16:
** ld1	{v0\.h}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_p16, poly16_t *, poly16x4_t)

/*
** test_vld1_lane_p64:
** ldr	d0, \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1_lane_p64, poly64_t *, poly64x1_t)

/*
** test_vld1q_lane_f16:
** ld1	{v0\.h}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_f16, float16_t *, float16x8_t)

/*
** test_vld1q_lane_bf16:
** ld1	{v0\.h}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_bf16, bfloat16_t *, bfloat16x8_t)

/*
** test_vld1q_lane_f32:
** ld1	{v0\.s}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_f32, float32_t *, float32x4_t)

/*
** test_vld1q_lane_f64:
** ld1	{v0\.d}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_f64, float64_t *, float64x2_t)

/*
** test_vld1q_lane_mf8:
** ld1	{v0\.b}\[15\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_mf8, mfloat8_t *, mfloat8x16_t)

/*
** test_vld1q_lane_s8:
** ld1	{v0\.b}\[15\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_s8, int8_t *, int8x16_t)

/*
** test_vld1q_lane_s16:
** ld1	{v0\.h}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_s16, int16_t *, int16x8_t)

/*
** test_vld1q_lane_s32:
** ld1	{v0\.s}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_s32, int32_t *, int32x4_t)

/*
** test_vld1q_lane_s64:
** ld1	{v0\.d}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_s64, int64_t *, int64x2_t)

/*
** test_vld1q_lane_u8:
** ld1	{v0\.b}\[15\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_u8, uint8_t *, uint8x16_t)

/*
** test_vld1q_lane_u16:
** ld1	{v0\.h}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_u16, uint16_t *, uint16x8_t)

/*
** test_vld1q_lane_u32:
** ld1	{v0\.s}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_u32, uint32_t *, uint32x4_t)

/*
** test_vld1q_lane_u64:
** ld1	{v0\.d}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_u64, uint64_t *, uint64x2_t)

/*
** test_vld1q_lane_p8:
** ld1	{v0\.b}\[15\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_p8, poly8_t *, poly8x16_t)

/*
** test_vld1q_lane_p16:
** ld1	{v0\.h}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_p16, poly16_t *, poly16x8_t)

/*
** test_vld1q_lane_p64:
** ld1	{v0\.d}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vld1q_lane_p64, poly64_t *, poly64x2_t)
