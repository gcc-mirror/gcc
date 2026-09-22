/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vld1_mf8:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_mf8, mfloat8x8_t, const mfloat8_t *)

/*
** test_vld1_s8:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_s8, int8x8_t, const int8_t *)

/*
** test_vld1_u8:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_u8, uint8x8_t, const uint8_t *)

/*
** test_vld1_s16:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_s16, int16x4_t, const int16_t *)

/*
** test_vld1_u16:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_u16, uint16x4_t, const uint16_t *)

/*
** test_vld1_s32:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_s32, int32x2_t, const int32_t *)

/*
** test_vld1_u32:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_u32, uint32x2_t, const uint32_t *)

/*
** test_vld1_s64:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_s64, int64x1_t, const int64_t *)

/*
** test_vld1_u64:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_u64, uint64x1_t, const uint64_t *)

/*
** test_vld1_p8:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_p8, poly8x8_t, const poly8_t *)

/*
** test_vld1_p16:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_p16, poly16x4_t, const poly16_t *)

/*
** test_vld1_p64:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_p64, poly64x1_t, const poly64_t *)

/*
** test_vld1_bf16:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_bf16, bfloat16x4_t, const bfloat16_t *)

/*
** test_vld1_f16:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_f16, float16x4_t, const float16_t *)

/*
** test_vld1_f32:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_f32, float32x2_t, const float32_t *)

/*
** test_vld1_f64:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_f64, float64x1_t, const float64_t *)

/*
** test_vld1q_mf8:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_mf8, mfloat8x16_t, const mfloat8_t *)

/*
** test_vld1q_s8:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s8, int8x16_t, const int8_t *)

/*
** test_vld1q_u8:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u8, uint8x16_t, const uint8_t *)

/*
** test_vld1q_s16:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s16, int16x8_t, const int16_t *)

/*
** test_vld1q_u16:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u16, uint16x8_t, const uint16_t *)

/*
** test_vld1q_s32:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s32, int32x4_t, const int32_t *)

/*
** test_vld1q_u32:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u32, uint32x4_t, const uint32_t *)

/*
** test_vld1q_s64:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s64, int64x2_t, const int64_t *)

/*
** test_vld1q_u64:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u64, uint64x2_t, const uint64_t *)

/*
** test_vld1q_p8:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_p8, poly8x16_t, const poly8_t *)

/*
** test_vld1q_p16:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_p16, poly16x8_t, const poly16_t *)

/*
** test_vld1q_p64:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_p64, poly64x2_t, const poly64_t *)

/*
** test_vld1q_bf16:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_bf16, bfloat16x8_t, const bfloat16_t *)

/*
** test_vld1q_f16:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_f16, float16x8_t, const float16_t *)

/*
** test_vld1q_f32:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_f32, float32x4_t, const float32_t *)

/*
** test_vld1q_f64:
** ldr	q0, \[x0\]
** ret
*/
TEST_UNARY (vld1q_f64, float64x2_t, const float64_t *)

/*
** test_vld1_mf8_x2:
** ld1	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_mf8_x2, mfloat8x8x2_t, const mfloat8_t *)

/*
** test_vld1_s8_x2:
** ld1	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_s8_x2, int8x8x2_t, const int8_t *)

/*
** test_vld1_u8_x2:
** ld1	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_u8_x2, uint8x8x2_t, const uint8_t *)

/*
** test_vld1_s16_x2:
** ld1	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_s16_x2, int16x4x2_t, const int16_t *)

/*
** test_vld1_u16_x2:
** ld1	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_u16_x2, uint16x4x2_t, const uint16_t *)

/*
** test_vld1_s32_x2:
** ld1	{v0\.2s - v1\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld1_s32_x2, int32x2x2_t, const int32_t *)

/*
** test_vld1_u32_x2:
** ld1	{v0\.2s - v1\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld1_u32_x2, uint32x2x2_t, const uint32_t *)

/*
** test_vld1_s64_x2:
** ld1	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld1_s64_x2, int64x1x2_t, const int64_t *)

/*
** test_vld1_u64_x2:
** ld1	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld1_u64_x2, uint64x1x2_t, const uint64_t *)

/*
** test_vld1_p8_x2:
** ld1	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_p8_x2, poly8x8x2_t, const poly8_t *)

/*
** test_vld1_p16_x2:
** ld1	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_p16_x2, poly16x4x2_t, const poly16_t *)

/*
** test_vld1_p64_x2:
** ld1	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld1_p64_x2, poly64x1x2_t, const poly64_t *)

/*
** test_vld1_bf16_x2:
** ld1	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_bf16_x2, bfloat16x4x2_t, const bfloat16_t *)

/*
** test_vld1_f16_x2:
** ld1	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_f16_x2, float16x4x2_t, const float16_t *)

/*
** test_vld1_f32_x2:
** ld1	{v0\.2s - v1\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld1_f32_x2, float32x2x2_t, const float32_t *)

/*
** test_vld1_f64_x2:
** ld1	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld1_f64_x2, float64x1x2_t, const float64_t *)

/*
** test_vld1q_mf8_x2:
** ld1	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_mf8_x2, mfloat8x16x2_t, const mfloat8_t *)

/*
** test_vld1q_s8_x2:
** ld1	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s8_x2, int8x16x2_t, const int8_t *)

/*
** test_vld1q_u8_x2:
** ld1	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u8_x2, uint8x16x2_t, const uint8_t *)

/*
** test_vld1q_s16_x2:
** ld1	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s16_x2, int16x8x2_t, const int16_t *)

/*
** test_vld1q_u16_x2:
** ld1	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u16_x2, uint16x8x2_t, const uint16_t *)

/*
** test_vld1q_s32_x2:
** ld1	{v0\.4s - v1\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s32_x2, int32x4x2_t, const int32_t *)

/*
** test_vld1q_u32_x2:
** ld1	{v0\.4s - v1\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u32_x2, uint32x4x2_t, const uint32_t *)

/*
** test_vld1q_s64_x2:
** ld1	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s64_x2, int64x2x2_t, const int64_t *)

/*
** test_vld1q_u64_x2:
** ld1	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u64_x2, uint64x2x2_t, const uint64_t *)

/*
** test_vld1q_p8_x2:
** ld1	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_p8_x2, poly8x16x2_t, const poly8_t *)

/*
** test_vld1q_p16_x2:
** ld1	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_p16_x2, poly16x8x2_t, const poly16_t *)

/*
** test_vld1q_p64_x2:
** ld1	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_p64_x2, poly64x2x2_t, const poly64_t *)

/*
** test_vld1q_bf16_x2:
** ld1	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_bf16_x2, bfloat16x8x2_t, const bfloat16_t *)

/*
** test_vld1q_f16_x2:
** ld1	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_f16_x2, float16x8x2_t, const float16_t *)

/*
** test_vld1q_f32_x2:
** ld1	{v0\.4s - v1\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_f32_x2, float32x4x2_t, const float32_t *)

/*
** test_vld1q_f64_x2:
** ld1	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_f64_x2, float64x2x2_t, const float64_t *)

/*
** test_vld1_mf8_x3:
** ld1	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_mf8_x3, mfloat8x8x3_t, const mfloat8_t *)

/*
** test_vld1_s8_x3:
** ld1	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_s8_x3, int8x8x3_t, const int8_t *)

/*
** test_vld1_u8_x3:
** ld1	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_u8_x3, uint8x8x3_t, const uint8_t *)

/*
** test_vld1_s16_x3:
** ld1	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_s16_x3, int16x4x3_t, const int16_t *)

/*
** test_vld1_u16_x3:
** ld1	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_u16_x3, uint16x4x3_t, const uint16_t *)

/*
** test_vld1_s32_x3:
** ld1	{v0\.2s - v2\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld1_s32_x3, int32x2x3_t, const int32_t *)

/*
** test_vld1_u32_x3:
** ld1	{v0\.2s - v2\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld1_u32_x3, uint32x2x3_t, const uint32_t *)

/*
** test_vld1_s64_x3:
** ld1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld1_s64_x3, int64x1x3_t, const int64_t *)

/*
** test_vld1_u64_x3:
** ld1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld1_u64_x3, uint64x1x3_t, const uint64_t *)

/*
** test_vld1_p8_x3:
** ld1	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_p8_x3, poly8x8x3_t, const poly8_t *)

/*
** test_vld1_p16_x3:
** ld1	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_p16_x3, poly16x4x3_t, const poly16_t *)

/*
** test_vld1_p64_x3:
** ld1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld1_p64_x3, poly64x1x3_t, const poly64_t *)

/*
** test_vld1_bf16_x3:
** ld1	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_bf16_x3, bfloat16x4x3_t, const bfloat16_t *)

/*
** test_vld1_f16_x3:
** ld1	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_f16_x3, float16x4x3_t, const float16_t *)

/*
** test_vld1_f32_x3:
** ld1	{v0\.2s - v2\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld1_f32_x3, float32x2x3_t, const float32_t *)

/*
** test_vld1_f64_x3:
** ld1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld1_f64_x3, float64x1x3_t, const float64_t *)

/*
** test_vld1q_mf8_x3:
** ld1	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_mf8_x3, mfloat8x16x3_t, const mfloat8_t *)

/*
** test_vld1q_s8_x3:
** ld1	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s8_x3, int8x16x3_t, const int8_t *)

/*
** test_vld1q_u8_x3:
** ld1	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u8_x3, uint8x16x3_t, const uint8_t *)

/*
** test_vld1q_s16_x3:
** ld1	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s16_x3, int16x8x3_t, const int16_t *)

/*
** test_vld1q_u16_x3:
** ld1	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u16_x3, uint16x8x3_t, const uint16_t *)

/*
** test_vld1q_s32_x3:
** ld1	{v0\.4s - v2\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s32_x3, int32x4x3_t, const int32_t *)

/*
** test_vld1q_u32_x3:
** ld1	{v0\.4s - v2\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u32_x3, uint32x4x3_t, const uint32_t *)

/*
** test_vld1q_s64_x3:
** ld1	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s64_x3, int64x2x3_t, const int64_t *)

/*
** test_vld1q_u64_x3:
** ld1	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u64_x3, uint64x2x3_t, const uint64_t *)

/*
** test_vld1q_p8_x3:
** ld1	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_p8_x3, poly8x16x3_t, const poly8_t *)

/*
** test_vld1q_p16_x3:
** ld1	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_p16_x3, poly16x8x3_t, const poly16_t *)

/*
** test_vld1q_p64_x3:
** ld1	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_p64_x3, poly64x2x3_t, const poly64_t *)

/*
** test_vld1q_bf16_x3:
** ld1	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_bf16_x3, bfloat16x8x3_t, const bfloat16_t *)

/*
** test_vld1q_f16_x3:
** ld1	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_f16_x3, float16x8x3_t, const float16_t *)

/*
** test_vld1q_f32_x3:
** ld1	{v0\.4s - v2\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_f32_x3, float32x4x3_t, const float32_t *)

/*
** test_vld1q_f64_x3:
** ld1	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_f64_x3, float64x2x3_t, const float64_t *)

/*
** test_vld1_mf8_x4:
** ld1	{v0\.8b - v3\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_mf8_x4, mfloat8x8x4_t, const mfloat8_t *)

/*
** test_vld1_s8_x4:
** ld1	{v0\.8b - v3\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_s8_x4, int8x8x4_t, const int8_t *)

/*
** test_vld1_u8_x4:
** ld1	{v0\.8b - v3\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_u8_x4, uint8x8x4_t, const uint8_t *)

/*
** test_vld1_s16_x4:
** ld1	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_s16_x4, int16x4x4_t, const int16_t *)

/*
** test_vld1_u16_x4:
** ld1	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_u16_x4, uint16x4x4_t, const uint16_t *)

/*
** test_vld1_s32_x4:
** ld1	{v0\.2s - v3\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld1_s32_x4, int32x2x4_t, const int32_t *)

/*
** test_vld1_u32_x4:
** ld1	{v0\.2s - v3\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld1_u32_x4, uint32x2x4_t, const uint32_t *)

/*
** test_vld1_s64_x4:
** ld1	{v0\.1d - v3\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld1_s64_x4, int64x1x4_t, const int64_t *)

/*
** test_vld1_u64_x4:
** ld1	{v0\.1d - v3\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld1_u64_x4, uint64x1x4_t, const uint64_t *)

/*
** test_vld1_p8_x4:
** ld1	{v0\.8b - v3\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_p8_x4, poly8x8x4_t, const poly8_t *)

/*
** test_vld1_p16_x4:
** ld1	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_p16_x4, poly16x4x4_t, const poly16_t *)

/*
** test_vld1_p64_x4:
** ld1	{v0\.1d - v3\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld1_p64_x4, poly64x1x4_t, const poly64_t *)

/*
** test_vld1_bf16_x4:
** ld1	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_bf16_x4, bfloat16x4x4_t, const bfloat16_t *)

/*
** test_vld1_f16_x4:
** ld1	{v0\.4h - v3\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_f16_x4, float16x4x4_t, const float16_t *)

/*
** test_vld1_f32_x4:
** ld1	{v0\.2s - v3\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld1_f32_x4, float32x2x4_t, const float32_t *)

/*
** test_vld1_f64_x4:
** ld1	{v0\.1d - v3\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld1_f64_x4, float64x1x4_t, const float64_t *)

/*
** test_vld1q_mf8_x4:
** ld1	{v0\.16b - v3\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_mf8_x4, mfloat8x16x4_t, const mfloat8_t *)

/*
** test_vld1q_s8_x4:
** ld1	{v0\.16b - v3\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s8_x4, int8x16x4_t, const int8_t *)

/*
** test_vld1q_u8_x4:
** ld1	{v0\.16b - v3\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u8_x4, uint8x16x4_t, const uint8_t *)

/*
** test_vld1q_s16_x4:
** ld1	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s16_x4, int16x8x4_t, const int16_t *)

/*
** test_vld1q_u16_x4:
** ld1	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u16_x4, uint16x8x4_t, const uint16_t *)

/*
** test_vld1q_s32_x4:
** ld1	{v0\.4s - v3\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s32_x4, int32x4x4_t, const int32_t *)

/*
** test_vld1q_u32_x4:
** ld1	{v0\.4s - v3\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u32_x4, uint32x4x4_t, const uint32_t *)

/*
** test_vld1q_s64_x4:
** ld1	{v0\.2d - v3\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_s64_x4, int64x2x4_t, const int64_t *)

/*
** test_vld1q_u64_x4:
** ld1	{v0\.2d - v3\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_u64_x4, uint64x2x4_t, const uint64_t *)

/*
** test_vld1q_p8_x4:
** ld1	{v0\.16b - v3\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_p8_x4, poly8x16x4_t, const poly8_t *)

/*
** test_vld1q_p16_x4:
** ld1	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_p16_x4, poly16x8x4_t, const poly16_t *)

/*
** test_vld1q_p64_x4:
** ld1	{v0\.2d - v3\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_p64_x4, poly64x2x4_t, const poly64_t *)

/*
** test_vld1q_bf16_x4:
** ld1	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_bf16_x4, bfloat16x8x4_t, const bfloat16_t *)

/*
** test_vld1q_f16_x4:
** ld1	{v0\.8h - v3\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_f16_x4, float16x8x4_t, const float16_t *)

/*
** test_vld1q_f32_x4:
** ld1	{v0\.4s - v3\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_f32_x4, float32x4x4_t, const float32_t *)

/*
** test_vld1q_f64_x4:
** ld1	{v0\.2d - v3\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_f64_x4, float64x2x4_t, const float64_t *)
