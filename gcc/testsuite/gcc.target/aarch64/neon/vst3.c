/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vst3_mf8:
** st3	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst3_mf8, mfloat8_t *, mfloat8x8x3_t)

/*
** test_vst3_s8:
** st3	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst3_s8, int8_t *, int8x8x3_t)

/*
** test_vst3_u8:
** st3	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst3_u8, uint8_t *, uint8x8x3_t)

/*
** test_vst3_s16:
** st3	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst3_s16, int16_t *, int16x4x3_t)

/*
** test_vst3_u16:
** st3	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst3_u16, uint16_t *, uint16x4x3_t)

/*
** test_vst3_s32:
** st3	{v0\.2s - v2\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst3_s32, int32_t *, int32x2x3_t)

/*
** test_vst3_u32:
** st3	{v0\.2s - v2\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst3_u32, uint32_t *, uint32x2x3_t)

/*
** test_vst3_s64:
** st1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst3_s64, int64_t *, int64x1x3_t)

/*
** test_vst3_u64:
** st1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst3_u64, uint64_t *, uint64x1x3_t)

/*
** test_vst3_p8:
** st3	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst3_p8, poly8_t *, poly8x8x3_t)

/*
** test_vst3_p16:
** st3	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst3_p16, poly16_t *, poly16x4x3_t)

/*
** test_vst3_p64:
** st1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst3_p64, poly64_t *, poly64x1x3_t)

/*
** test_vst3_bf16:
** st3	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst3_bf16, bfloat16_t *, bfloat16x4x3_t)

/*
** test_vst3_f16:
** st3	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst3_f16, float16_t *, float16x4x3_t)

/*
** test_vst3_f32:
** st3	{v0\.2s - v2\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst3_f32, float32_t *, float32x2x3_t)

/*
** test_vst3_f64:
** st1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst3_f64, float64_t *, float64x1x3_t)

/*
** test_vst3q_mf8:
** st3	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst3q_mf8, mfloat8_t *, mfloat8x16x3_t)

/*
** test_vst3q_s8:
** st3	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst3q_s8, int8_t *, int8x16x3_t)

/*
** test_vst3q_u8:
** st3	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst3q_u8, uint8_t *, uint8x16x3_t)

/*
** test_vst3q_s16:
** st3	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst3q_s16, int16_t *, int16x8x3_t)

/*
** test_vst3q_u16:
** st3	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst3q_u16, uint16_t *, uint16x8x3_t)

/*
** test_vst3q_s32:
** st3	{v0\.4s - v2\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst3q_s32, int32_t *, int32x4x3_t)

/*
** test_vst3q_u32:
** st3	{v0\.4s - v2\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst3q_u32, uint32_t *, uint32x4x3_t)

/*
** test_vst3q_s64:
** st3	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst3q_s64, int64_t *, int64x2x3_t)

/*
** test_vst3q_u64:
** st3	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst3q_u64, uint64_t *, uint64x2x3_t)

/*
** test_vst3q_p8:
** st3	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst3q_p8, poly8_t *, poly8x16x3_t)

/*
** test_vst3q_p16:
** st3	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst3q_p16, poly16_t *, poly16x8x3_t)

/*
** test_vst3q_p64:
** st3	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst3q_p64, poly64_t *, poly64x2x3_t)

/*
** test_vst3q_bf16:
** st3	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst3q_bf16, bfloat16_t *, bfloat16x8x3_t)

/*
** test_vst3q_f16:
** st3	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst3q_f16, float16_t *, float16x8x3_t)

/*
** test_vst3q_f32:
** st3	{v0\.4s - v2\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst3q_f32, float32_t *, float32x4x3_t)

/*
** test_vst3q_f64:
** st3	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst3q_f64, float64_t *, float64x2x3_t)
