/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vst2_mf8:
** st2	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst2_mf8, mfloat8_t *, mfloat8x8x2_t)

/*
** test_vst2_s8:
** st2	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst2_s8, int8_t *, int8x8x2_t)

/*
** test_vst2_u8:
** st2	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst2_u8, uint8_t *, uint8x8x2_t)

/*
** test_vst2_s16:
** st2	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst2_s16, int16_t *, int16x4x2_t)

/*
** test_vst2_u16:
** st2	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst2_u16, uint16_t *, uint16x4x2_t)

/*
** test_vst2_s32:
** st2	{v0\.2s - v1\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst2_s32, int32_t *, int32x2x2_t)

/*
** test_vst2_u32:
** st2	{v0\.2s - v1\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst2_u32, uint32_t *, uint32x2x2_t)

/*
** test_vst2_s64:
** st1	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst2_s64, int64_t *, int64x1x2_t)

/*
** test_vst2_u64:
** st1	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst2_u64, uint64_t *, uint64x1x2_t)

/*
** test_vst2_p8:
** st2	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst2_p8, poly8_t *, poly8x8x2_t)

/*
** test_vst2_p16:
** st2	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst2_p16, poly16_t *, poly16x4x2_t)

/*
** test_vst2_p64:
** st1	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst2_p64, poly64_t *, poly64x1x2_t)

/*
** test_vst2_bf16:
** st2	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst2_bf16, bfloat16_t *, bfloat16x4x2_t)

/*
** test_vst2_f16:
** st2	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst2_f16, float16_t *, float16x4x2_t)

/*
** test_vst2_f32:
** st2	{v0\.2s - v1\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst2_f32, float32_t *, float32x2x2_t)

/*
** test_vst2_f64:
** st1	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst2_f64, float64_t *, float64x1x2_t)

/*
** test_vst2q_mf8:
** st2	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst2q_mf8, mfloat8_t *, mfloat8x16x2_t)

/*
** test_vst2q_s8:
** st2	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst2q_s8, int8_t *, int8x16x2_t)

/*
** test_vst2q_u8:
** st2	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst2q_u8, uint8_t *, uint8x16x2_t)

/*
** test_vst2q_s16:
** st2	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst2q_s16, int16_t *, int16x8x2_t)

/*
** test_vst2q_u16:
** st2	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst2q_u16, uint16_t *, uint16x8x2_t)

/*
** test_vst2q_s32:
** st2	{v0\.4s - v1\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst2q_s32, int32_t *, int32x4x2_t)

/*
** test_vst2q_u32:
** st2	{v0\.4s - v1\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst2q_u32, uint32_t *, uint32x4x2_t)

/*
** test_vst2q_s64:
** st2	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst2q_s64, int64_t *, int64x2x2_t)

/*
** test_vst2q_u64:
** st2	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst2q_u64, uint64_t *, uint64x2x2_t)

/*
** test_vst2q_p8:
** st2	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst2q_p8, poly8_t *, poly8x16x2_t)

/*
** test_vst2q_p16:
** st2	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst2q_p16, poly16_t *, poly16x8x2_t)

/*
** test_vst2q_p64:
** st2	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst2q_p64, poly64_t *, poly64x2x2_t)

/*
** test_vst2q_bf16:
** st2	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst2q_bf16, bfloat16_t *, bfloat16x8x2_t)

/*
** test_vst2q_f16:
** st2	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst2q_f16, float16_t *, float16x8x2_t)

/*
** test_vst2q_f32:
** st2	{v0\.4s - v1\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst2q_f32, float32_t *, float32x4x2_t)

/*
** test_vst2q_f64:
** st2	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst2q_f64, float64_t *, float64x2x2_t)
