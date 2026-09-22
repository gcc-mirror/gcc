/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vld3_lane_bf16:
** ld3	{v0\.h - v2\.h}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_bf16, bfloat16_t *, bfloat16x4x3_t)

/*
** test_vld3_lane_f16:
** ld3	{v0\.h - v2\.h}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_f16, float16_t *, float16x4x3_t)

/*
** test_vld3_lane_f32:
** ld3	{v0\.s - v2\.s}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_f32, float32_t *, float32x2x3_t)

/*
** test_vld3_lane_f64:
** ld3	{v0\.d - v2\.d}\[0\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_f64, float64_t *, float64x1x3_t)

/*
** test_vld3_lane_mf8:
** ld3	{v0\.b - v2\.b}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_mf8, mfloat8_t *, mfloat8x8x3_t)

/*
** test_vld3_lane_s8:
** ld3	{v0\.b - v2\.b}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_s8, int8_t *, int8x8x3_t)

/*
** test_vld3_lane_s16:
** ld3	{v0\.h - v2\.h}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_s16, int16_t *, int16x4x3_t)

/*
** test_vld3_lane_s32:
** ld3	{v0\.s - v2\.s}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_s32, int32_t *, int32x2x3_t)

/*
** test_vld3_lane_s64:
** ld3	{v0\.d - v2\.d}\[0\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_s64, int64_t *, int64x1x3_t)

/*
** test_vld3_lane_u8:
** ld3	{v0\.b - v2\.b}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_u8, uint8_t *, uint8x8x3_t)

/*
** test_vld3_lane_u16:
** ld3	{v0\.h - v2\.h}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_u16, uint16_t *, uint16x4x3_t)

/*
** test_vld3_lane_u32:
** ld3	{v0\.s - v2\.s}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_u32, uint32_t *, uint32x2x3_t)

/*
** test_vld3_lane_u64:
** ld3	{v0\.d - v2\.d}\[0\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_u64, uint64_t *, uint64x1x3_t)

/*
** test_vld3_lane_p8:
** ld3	{v0\.b - v2\.b}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_p8, poly8_t *, poly8x8x3_t)

/*
** test_vld3_lane_p16:
** ld3	{v0\.h - v2\.h}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_p16, poly16_t *, poly16x4x3_t)

/*
** test_vld3_lane_p64:
** ld3	{v0\.d - v2\.d}\[0\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3_lane_p64, poly64_t *, poly64x1x3_t)

/*
** test_vld3q_lane_f16:
** ld3	{v0\.h - v2\.h}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_f16, float16_t *, float16x8x3_t)

/*
** test_vld3q_lane_bf16:
** ld3	{v0\.h - v2\.h}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_bf16, bfloat16_t *, bfloat16x8x3_t)

/*
** test_vld3q_lane_f32:
** ld3	{v0\.s - v2\.s}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_f32, float32_t *, float32x4x3_t)

/*
** test_vld3q_lane_f64:
** ld3	{v0\.d - v2\.d}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_f64, float64_t *, float64x2x3_t)

/*
** test_vld3q_lane_mf8:
** ld3	{v0\.b - v2\.b}\[15\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_mf8, mfloat8_t *, mfloat8x16x3_t)

/*
** test_vld3q_lane_s8:
** ld3	{v0\.b - v2\.b}\[15\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_s8, int8_t *, int8x16x3_t)

/*
** test_vld3q_lane_s16:
** ld3	{v0\.h - v2\.h}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_s16, int16_t *, int16x8x3_t)

/*
** test_vld3q_lane_s32:
** ld3	{v0\.s - v2\.s}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_s32, int32_t *, int32x4x3_t)

/*
** test_vld3q_lane_s64:
** ld3	{v0\.d - v2\.d}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_s64, int64_t *, int64x2x3_t)

/*
** test_vld3q_lane_u8:
** ld3	{v0\.b - v2\.b}\[15\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_u8, uint8_t *, uint8x16x3_t)

/*
** test_vld3q_lane_u16:
** ld3	{v0\.h - v2\.h}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_u16, uint16_t *, uint16x8x3_t)

/*
** test_vld3q_lane_u32:
** ld3	{v0\.s - v2\.s}\[3\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_u32, uint32_t *, uint32x4x3_t)

/*
** test_vld3q_lane_u64:
** ld3	{v0\.d - v2\.d}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_u64, uint64_t *, uint64x2x3_t)

/*
** test_vld3q_lane_p8:
** ld3	{v0\.b - v2\.b}\[15\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_p8, poly8_t *, poly8x16x3_t)

/*
** test_vld3q_lane_p16:
** ld3	{v0\.h - v2\.h}\[7\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_p16, poly16_t *, poly16x8x3_t)

/*
** test_vld3q_lane_p64:
** ld3	{v0\.d - v2\.d}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE_TUPLE (vld3q_lane_p64, poly64_t *, poly64x2x3_t)
