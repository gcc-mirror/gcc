/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=armv9-a+lut" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

#define TEST_LUTI2(NAME, RET_TYPE, ARG_1_TYPE, ARG_2_TYPE)                    \
  RET_TYPE test_##NAME (ARG_1_TYPE vn, ARG_2_TYPE vm)                         \
  {                                                                           \
    return NAME (vn, vm, 0);                                                   \
  }

/*
** test_vluti2_lane_u8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_lane_u8, uint8x16_t, uint8x8_t, uint8x8_t)

/*
** test_vluti2_laneq_u8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_laneq_u8, uint8x16_t, uint8x8_t, uint8x16_t)

/*
** test_vluti2q_lane_u8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_lane_u8, uint8x16_t, uint8x16_t, uint8x8_t)

/*
** test_vluti2q_laneq_u8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_laneq_u8, uint8x16_t, uint8x16_t, uint8x16_t)

/*
** test_vluti2_lane_s8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_lane_s8, int8x16_t, int8x8_t, uint8x8_t)

/*
** test_vluti2_laneq_s8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_laneq_s8, int8x16_t, int8x8_t, uint8x16_t)

/*
** test_vluti2q_lane_s8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_lane_s8, int8x16_t, int8x16_t, uint8x8_t)

/*
** test_vluti2q_laneq_s8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_laneq_s8, int8x16_t, int8x16_t, uint8x16_t)

/*
** test_vluti2_lane_p8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_lane_p8, poly8x16_t, poly8x8_t, uint8x8_t)

/*
** test_vluti2_laneq_p8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_laneq_p8, poly8x16_t, poly8x8_t, uint8x16_t)

/*
** test_vluti2q_lane_p8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_lane_p8, poly8x16_t, poly8x16_t, uint8x8_t)

/*
** test_vluti2q_laneq_p8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_laneq_p8, poly8x16_t, poly8x16_t, uint8x16_t)

/*
** test_vluti2_lane_mf8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_lane_mf8, mfloat8x16_t, mfloat8x8_t, uint8x8_t)

/*
** test_vluti2_laneq_mf8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_laneq_mf8, mfloat8x16_t, mfloat8x8_t, uint8x16_t)

/*
** test_vluti2q_lane_mf8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_lane_mf8, mfloat8x16_t, mfloat8x16_t, uint8x8_t)

/*
** test_vluti2q_laneq_mf8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_laneq_mf8, mfloat8x16_t, mfloat8x16_t, uint8x16_t)

/*
** test_vluti2_lane_u16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_lane_u16, uint16x8_t, uint16x4_t, uint8x8_t)

/*
** test_vluti2_laneq_u16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_laneq_u16, uint16x8_t, uint16x4_t, uint8x16_t)

/*
** test_vluti2q_lane_u16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_lane_u16, uint16x8_t, uint16x8_t, uint8x8_t)

/*
** test_vluti2q_laneq_u16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_laneq_u16, uint16x8_t, uint16x8_t, uint8x16_t)

/*
** test_vluti2_lane_s16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_lane_s16, int16x8_t, int16x4_t, uint8x8_t)

/*
** test_vluti2_laneq_s16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_laneq_s16, int16x8_t, int16x4_t, uint8x16_t)

/*
** test_vluti2q_lane_s16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_lane_s16, int16x8_t, int16x8_t, uint8x8_t)

/*
** test_vluti2q_laneq_s16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_laneq_s16, int16x8_t, int16x8_t, uint8x16_t)

/*
** test_vluti2_lane_f16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_lane_f16, float16x8_t, float16x4_t, uint8x8_t)

/*
** test_vluti2_laneq_f16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_laneq_f16, float16x8_t, float16x4_t, uint8x16_t)

/*
** test_vluti2q_lane_f16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_lane_f16, float16x8_t, float16x8_t, uint8x8_t)

/*
** test_vluti2q_laneq_f16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_laneq_f16, float16x8_t, float16x8_t, uint8x16_t)

/*
** test_vluti2_lane_bf16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_lane_bf16, bfloat16x8_t, bfloat16x4_t, uint8x8_t)

/*
** test_vluti2_laneq_bf16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_laneq_bf16, bfloat16x8_t, bfloat16x4_t, uint8x16_t)

/*
** test_vluti2q_lane_bf16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_lane_bf16, bfloat16x8_t, bfloat16x8_t, uint8x8_t)

/*
** test_vluti2q_laneq_bf16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_laneq_bf16, bfloat16x8_t, bfloat16x8_t, uint8x16_t)

/*
** test_vluti2_lane_p16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_lane_p16, poly16x8_t, poly16x4_t, uint8x8_t)

/*
** test_vluti2_laneq_p16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2_laneq_p16, poly16x8_t, poly16x4_t, uint8x16_t)

/*
** test_vluti2q_lane_p16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_lane_p16, poly16x8_t, poly16x8_t, uint8x8_t)

/*
** test_vluti2q_laneq_p16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	ret
*/
TEST_LUTI2 (vluti2q_laneq_p16, poly16x8_t, poly16x8_t, uint8x16_t)
