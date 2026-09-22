/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vst1_mf8:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_mf8, mfloat8_t *, mfloat8x8_t)

/*
** test_vst1_s8:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_s8, int8_t *, int8x8_t)

/*
** test_vst1_u8:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_u8, uint8_t *, uint8x8_t)

/*
** test_vst1_s16:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_s16, int16_t *, int16x4_t)

/*
** test_vst1_u16:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_u16, uint16_t *, uint16x4_t)

/*
** test_vst1_s32:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_s32, int32_t *, int32x2_t)

/*
** test_vst1_u32:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_u32, uint32_t *, uint32x2_t)

/*
** test_vst1_s64:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_s64, int64_t *, int64x1_t)

/*
** test_vst1_u64:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_u64, uint64_t *, uint64x1_t)

/*
** test_vst1_p8:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_p8, poly8_t *, poly8x8_t)

/*
** test_vst1_p16:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_p16, poly16_t *, poly16x4_t)

/*
** test_vst1_p64:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_p64, poly64_t *, poly64x1_t)

/*
** test_vst1_bf16:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_bf16, bfloat16_t *, bfloat16x4_t)

/*
** test_vst1_f16:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_f16, float16_t *, float16x4_t)

/*
** test_vst1_f32:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_f32, float32_t *, float32x2_t)

/*
** test_vst1_f64:
** str	d0, \[x0\]
** ret
*/
TEST_STORE (vst1_f64, float64_t *, float64x1_t)

/*
** test_vst1q_mf8:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_mf8, mfloat8_t *, mfloat8x16_t)

/*
** test_vst1q_s8:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_s8, int8_t *, int8x16_t)

/*
** test_vst1q_u8:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_u8, uint8_t *, uint8x16_t)

/*
** test_vst1q_s16:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_s16, int16_t *, int16x8_t)

/*
** test_vst1q_u16:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_u16, uint16_t *, uint16x8_t)

/*
** test_vst1q_s32:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_s32, int32_t *, int32x4_t)

/*
** test_vst1q_u32:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_u32, uint32_t *, uint32x4_t)

/*
** test_vst1q_s64:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_s64, int64_t *, int64x2_t)

/*
** test_vst1q_u64:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_u64, uint64_t *, uint64x2_t)

/*
** test_vst1q_p8:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_p8, poly8_t *, poly8x16_t)

/*
** test_vst1q_p16:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_p16, poly16_t *, poly16x8_t)

/*
** test_vst1q_p64:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_p64, poly64_t *, poly64x2_t)

/*
** test_vst1q_bf16:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_bf16, bfloat16_t *, bfloat16x8_t)

/*
** test_vst1q_f16:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_f16, float16_t *, float16x8_t)

/*
** test_vst1q_f32:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_f32, float32_t *, float32x4_t)

/*
** test_vst1q_f64:
** str	q0, \[x0\]
** ret
*/
TEST_STORE (vst1q_f64, float64_t *, float64x2_t)

/*
** test_vst1_s8_x2:
** st1	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst1_s8_x2, int8_t *, int8x8x2_t)

/*
** test_vst1_u8_x2:
** st1	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst1_u8_x2, uint8_t *, uint8x8x2_t)

/*
** test_vst1_s16_x2:
** st1	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_s16_x2, int16_t *, int16x4x2_t)

/*
** test_vst1_u16_x2:
** st1	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_u16_x2, uint16_t *, uint16x4x2_t)

/*
** test_vst1_s32_x2:
** st1	{v0\.2s - v1\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst1_s32_x2, int32_t *, int32x2x2_t)

/*
** test_vst1_u32_x2:
** st1	{v0\.2s - v1\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst1_u32_x2, uint32_t *, uint32x2x2_t)

/*
** test_vst1_s64_x2:
** st1	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst1_s64_x2, int64_t *, int64x1x2_t)

/*
** test_vst1_u64_x2:
** st1	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst1_u64_x2, uint64_t *, uint64x1x2_t)

/*
** test_vst1_p8_x2:
** st1	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst1_p8_x2, poly8_t *, poly8x8x2_t)

/*
** test_vst1_p16_x2:
** st1	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_p16_x2, poly16_t *, poly16x4x2_t)

/*
** test_vst1_p64_x2:
** st1	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst1_p64_x2, poly64_t *, poly64x1x2_t)

/*
** test_vst1_bf16_x2:
** st1	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_bf16_x2, bfloat16_t *, bfloat16x4x2_t)

/*
** test_vst1_f16_x2:
** st1	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_f16_x2, float16_t *, float16x4x2_t)

/*
** test_vst1_f32_x2:
** st1	{v0\.2s - v1\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst1_f32_x2, float32_t *, float32x2x2_t)

/*
** test_vst1_f64_x2:
** st1	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst1_f64_x2, float64_t *, float64x1x2_t)

/*
** test_vst1q_s8_x2:
** st1	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst1q_s8_x2, int8_t *, int8x16x2_t)

/*
** test_vst1q_u8_x2:
** st1	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst1q_u8_x2, uint8_t *, uint8x16x2_t)

/*
** test_vst1q_s16_x2:
** st1	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_s16_x2, int16_t *, int16x8x2_t)

/*
** test_vst1q_u16_x2:
** st1	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_u16_x2, uint16_t *, uint16x8x2_t)

/*
** test_vst1q_s32_x2:
** st1	{v0\.4s - v1\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst1q_s32_x2, int32_t *, int32x4x2_t)

/*
** test_vst1q_u32_x2:
** st1	{v0\.4s - v1\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst1q_u32_x2, uint32_t *, uint32x4x2_t)

/*
** test_vst1q_s64_x2:
** st1	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst1q_s64_x2, int64_t *, int64x2x2_t)

/*
** test_vst1q_u64_x2:
** st1	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst1q_u64_x2, uint64_t *, uint64x2x2_t)

/*
** test_vst1q_p8_x2:
** st1	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst1q_p8_x2, poly8_t *, poly8x16x2_t)

/*
** test_vst1q_p16_x2:
** st1	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_p16_x2, poly16_t *, poly16x8x2_t)

/*
** test_vst1q_p64_x2:
** st1	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst1q_p64_x2, poly64_t *, poly64x2x2_t)

/*
** test_vst1q_bf16_x2:
** st1	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_bf16_x2, bfloat16_t *, bfloat16x8x2_t)

/*
** test_vst1q_f16_x2:
** st1	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_f16_x2, float16_t *, float16x8x2_t)

/*
** test_vst1q_f32_x2:
** st1	{v0\.4s - v1\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst1q_f32_x2, float32_t *, float32x4x2_t)

/*
** test_vst1q_f64_x2:
** st1	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst1q_f64_x2, float64_t *, float64x2x2_t)

/*
** test_vst1_s8_x3:
** st1	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst1_s8_x3, int8_t *, int8x8x3_t)

/*
** test_vst1_u8_x3:
** st1	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst1_u8_x3, uint8_t *, uint8x8x3_t)

/*
** test_vst1_s16_x3:
** st1	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_s16_x3, int16_t *, int16x4x3_t)

/*
** test_vst1_u16_x3:
** st1	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_u16_x3, uint16_t *, uint16x4x3_t)

/*
** test_vst1_s32_x3:
** st1	{v0\.2s - v2\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst1_s32_x3, int32_t *, int32x2x3_t)

/*
** test_vst1_u32_x3:
** st1	{v0\.2s - v2\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst1_u32_x3, uint32_t *, uint32x2x3_t)

/*
** test_vst1_s64_x3:
** st1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst1_s64_x3, int64_t *, int64x1x3_t)

/*
** test_vst1_u64_x3:
** st1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst1_u64_x3, uint64_t *, uint64x1x3_t)

/*
** test_vst1_p8_x3:
** st1	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst1_p8_x3, poly8_t *, poly8x8x3_t)

/*
** test_vst1_p16_x3:
** st1	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_p16_x3, poly16_t *, poly16x4x3_t)

/*
** test_vst1_p64_x3:
** st1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst1_p64_x3, poly64_t *, poly64x1x3_t)

/*
** test_vst1_bf16_x3:
** st1	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_bf16_x3, bfloat16_t *, bfloat16x4x3_t)

/*
** test_vst1_f16_x3:
** st1	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_f16_x3, float16_t *, float16x4x3_t)

/*
** test_vst1_f32_x3:
** st1	{v0\.2s - v2\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst1_f32_x3, float32_t *, float32x2x3_t)

/*
** test_vst1_f64_x3:
** st1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst1_f64_x3, float64_t *, float64x1x3_t)

/*
** test_vst1q_s8_x3:
** st1	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst1q_s8_x3, int8_t *, int8x16x3_t)

/*
** test_vst1q_u8_x3:
** st1	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst1q_u8_x3, uint8_t *, uint8x16x3_t)

/*
** test_vst1q_s16_x3:
** st1	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_s16_x3, int16_t *, int16x8x3_t)

/*
** test_vst1q_u16_x3:
** st1	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_u16_x3, uint16_t *, uint16x8x3_t)

/*
** test_vst1q_s32_x3:
** st1	{v0\.4s - v2\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst1q_s32_x3, int32_t *, int32x4x3_t)

/*
** test_vst1q_u32_x3:
** st1	{v0\.4s - v2\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst1q_u32_x3, uint32_t *, uint32x4x3_t)

/*
** test_vst1q_s64_x3:
** st1	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst1q_s64_x3, int64_t *, int64x2x3_t)

/*
** test_vst1q_u64_x3:
** st1	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst1q_u64_x3, uint64_t *, uint64x2x3_t)

/*
** test_vst1q_p8_x3:
** st1	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst1q_p8_x3, poly8_t *, poly8x16x3_t)

/*
** test_vst1q_p16_x3:
** st1	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_p16_x3, poly16_t *, poly16x8x3_t)

/*
** test_vst1q_p64_x3:
** st1	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst1q_p64_x3, poly64_t *, poly64x2x3_t)

/*
** test_vst1q_bf16_x3:
** st1	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_bf16_x3, bfloat16_t *, bfloat16x8x3_t)

/*
** test_vst1q_f16_x3:
** st1	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_f16_x3, float16_t *, float16x8x3_t)

/*
** test_vst1q_f32_x3:
** st1	{v0\.4s - v2\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst1q_f32_x3, float32_t *, float32x4x3_t)

/*
** test_vst1q_f64_x3:
** st1	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst1q_f64_x3, float64_t *, float64x2x3_t)

/*
** test_vst1_s8_x4:
** st1	{v0\.8b - v3\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst1_s8_x4, int8_t *, int8x8x4_t)

/*
** test_vst1_u8_x4:
** st1	{v0\.8b - v3\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst1_u8_x4, uint8_t *, uint8x8x4_t)

/*
** test_vst1_s16_x4:
** st1	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_s16_x4, int16_t *, int16x4x4_t)

/*
** test_vst1_u16_x4:
** st1	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_u16_x4, uint16_t *, uint16x4x4_t)

/*
** test_vst1_s32_x4:
** st1	{v0\.2s - v3\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst1_s32_x4, int32_t *, int32x2x4_t)

/*
** test_vst1_u32_x4:
** st1	{v0\.2s - v3\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst1_u32_x4, uint32_t *, uint32x2x4_t)

/*
** test_vst1_s64_x4:
** st1	{v0\.1d - v3\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst1_s64_x4, int64_t *, int64x1x4_t)

/*
** test_vst1_u64_x4:
** st1	{v0\.1d - v3\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst1_u64_x4, uint64_t *, uint64x1x4_t)

/*
** test_vst1_p8_x4:
** st1	{v0\.8b - v3\.8b}, \[x0\]
** ret
*/
TEST_STORE (vst1_p8_x4, poly8_t *, poly8x8x4_t)

/*
** test_vst1_p16_x4:
** st1	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_p16_x4, poly16_t *, poly16x4x4_t)

/*
** test_vst1_p64_x4:
** st1	{v0\.1d - v3\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst1_p64_x4, poly64_t *, poly64x1x4_t)

/*
** test_vst1_bf16_x4:
** st1	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_bf16_x4, bfloat16_t *, bfloat16x4x4_t)

/*
** test_vst1_f16_x4:
** st1	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_STORE (vst1_f16_x4, float16_t *, float16x4x4_t)

/*
** test_vst1_f32_x4:
** st1	{v0\.2s - v3\.2s}, \[x0\]
** ret
*/
TEST_STORE (vst1_f32_x4, float32_t *, float32x2x4_t)

/*
** test_vst1_f64_x4:
** st1	{v0\.1d - v3\.1d}, \[x0\]
** ret
*/
TEST_STORE (vst1_f64_x4, float64_t *, float64x1x4_t)

/*
** test_vst1q_s8_x4:
** st1	{v0\.16b - v3\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst1q_s8_x4, int8_t *, int8x16x4_t)

/*
** test_vst1q_u8_x4:
** st1	{v0\.16b - v3\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst1q_u8_x4, uint8_t *, uint8x16x4_t)

/*
** test_vst1q_s16_x4:
** st1	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_s16_x4, int16_t *, int16x8x4_t)

/*
** test_vst1q_u16_x4:
** st1	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_u16_x4, uint16_t *, uint16x8x4_t)

/*
** test_vst1q_s32_x4:
** st1	{v0\.4s - v3\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst1q_s32_x4, int32_t *, int32x4x4_t)

/*
** test_vst1q_u32_x4:
** st1	{v0\.4s - v3\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst1q_u32_x4, uint32_t *, uint32x4x4_t)

/*
** test_vst1q_s64_x4:
** st1	{v0\.2d - v3\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst1q_s64_x4, int64_t *, int64x2x4_t)

/*
** test_vst1q_u64_x4:
** st1	{v0\.2d - v3\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst1q_u64_x4, uint64_t *, uint64x2x4_t)

/*
** test_vst1q_p8_x4:
** st1	{v0\.16b - v3\.16b}, \[x0\]
** ret
*/
TEST_STORE (vst1q_p8_x4, poly8_t *, poly8x16x4_t)

/*
** test_vst1q_p16_x4:
** st1	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_p16_x4, poly16_t *, poly16x8x4_t)

/*
** test_vst1q_p64_x4:
** st1	{v0\.2d - v3\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst1q_p64_x4, poly64_t *, poly64x2x4_t)

/*
** test_vst1q_bf16_x4:
** st1	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_bf16_x4, bfloat16_t *, bfloat16x8x4_t)

/*
** test_vst1q_f16_x4:
** st1	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_STORE (vst1q_f16_x4, float16_t *, float16x8x4_t)

/*
** test_vst1q_f32_x4:
** st1	{v0\.4s - v3\.4s}, \[x0\]
** ret
*/
TEST_STORE (vst1q_f32_x4, float32_t *, float32x4x4_t)

/*
** test_vst1q_f64_x4:
** st1	{v0\.2d - v3\.2d}, \[x0\]
** ret
*/
TEST_STORE (vst1q_f64_x4, float64_t *, float64x2x4_t)
