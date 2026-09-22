/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vst4_mf8:
** st4	{v0\.8b - v3\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst4_mf8, mfloat8_t *, mfloat8x8x4_t)

/*
** test_vst4_s8:
** st4	{v0\.8b - v3\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst4_s8, int8_t *, int8x8x4_t)

/*
** test_vst4_u8:
** st4	{v0\.8b - v3\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst4_u8, uint8_t *, uint8x8x4_t)

/*
** test_vst4_s16:
** st4	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst4_s16, int16_t *, int16x4x4_t)

/*
** test_vst4_u16:
** st4	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst4_u16, uint16_t *, uint16x4x4_t)

/*
** test_vst4_s32:
** st4	{v0\.2s - v3\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst4_s32, int32_t *, int32x2x4_t)

/*
** test_vst4_u32:
** st4	{v0\.2s - v3\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst4_u32, uint32_t *, uint32x2x4_t)

/*
** test_vst4_s64:
** st1	{v0\.1d - v3\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst4_s64, int64_t *, int64x1x4_t)

/*
** test_vst4_u64:
** st1	{v0\.1d - v3\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst4_u64, uint64_t *, uint64x1x4_t)

/*
** test_vst4_p8:
** st4	{v0\.8b - v3\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst4_p8, poly8_t *, poly8x8x4_t)

/*
** test_vst4_p16:
** st4	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst4_p16, poly16_t *, poly16x4x4_t)

/*
** test_vst4_p64:
** st1	{v0\.1d - v3\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst4_p64, poly64_t *, poly64x1x4_t)

/*
** test_vst4_bf16:
** st4	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst4_bf16, bfloat16_t *, bfloat16x4x4_t)

/*
** test_vst4_f16:
** st4	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst4_f16, float16_t *, float16x4x4_t)

/*
** test_vst4_f32:
** st4	{v0\.2s - v3\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst4_f32, float32_t *, float32x2x4_t)

/*
** test_vst4_f64:
** st1	{v0\.1d - v3\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst4_f64, float64_t *, float64x1x4_t)

/*
** test_vst4q_mf8:
** st4	{v0\.16b - v3\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst4q_mf8, mfloat8_t *, mfloat8x16x4_t)

/*
** test_vst4q_s8:
** st4	{v0\.16b - v3\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst4q_s8, int8_t *, int8x16x4_t)

/*
** test_vst4q_u8:
** st4	{v0\.16b - v3\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst4q_u8, uint8_t *, uint8x16x4_t)

/*
** test_vst4q_s16:
** st4	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst4q_s16, int16_t *, int16x8x4_t)

/*
** test_vst4q_u16:
** st4	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst4q_u16, uint16_t *, uint16x8x4_t)

/*
** test_vst4q_s32:
** st4	{v0\.4s - v3\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst4q_s32, int32_t *, int32x4x4_t)

/*
** test_vst4q_u32:
** st4	{v0\.4s - v3\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst4q_u32, uint32_t *, uint32x4x4_t)

/*
** test_vst4q_s64:
** st4	{v0\.2d - v3\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst4q_s64, int64_t *, int64x2x4_t)

/*
** test_vst4q_u64:
** st4	{v0\.2d - v3\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst4q_u64, uint64_t *, uint64x2x4_t)

/*
** test_vst4q_p8:
** st4	{v0\.16b - v3\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst4q_p8, poly8_t *, poly8x16x4_t)

/*
** test_vst4q_p16:
** st4	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst4q_p16, poly16_t *, poly16x8x4_t)

/*
** test_vst4q_p64:
** st4	{v0\.2d - v3\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst4q_p64, poly64_t *, poly64x2x4_t)

/*
** test_vst4q_bf16:
** st4	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst4q_bf16, bfloat16_t *, bfloat16x8x4_t)

/*
** test_vst4q_f16:
** st4	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst4q_f16, float16_t *, float16x8x4_t)

/*
** test_vst4q_f32:
** st4	{v0\.4s - v3\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst4q_f32, float32_t *, float32x4x4_t)

/*
** test_vst4q_f64:
** st4	{v0\.2d - v3\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst4q_f64, float64_t *, float64x2x4_t)
