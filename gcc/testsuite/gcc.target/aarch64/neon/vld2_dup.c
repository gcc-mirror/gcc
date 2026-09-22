/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vld2_dup_f16:
** ld2r	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_f16, float16x4x2_t, const float16_t *)

/*
** test_vld2_dup_bf16:
** ld2r	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_bf16, bfloat16x4x2_t, const bfloat16_t *)

/*
** test_vld2_dup_f32:
** ld2r	{v0\.2s - v1\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_f32, float32x2x2_t, const float32_t *)

/*
** test_vld2_dup_f64:
** ld2r	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_f64, float64x1x2_t, const float64_t *)

/*
** test_vld2_dup_mf8:
** ld2r	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_mf8, mfloat8x8x2_t, const mfloat8_t *)

/*
** test_vld2_dup_s8:
** ld2r	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_s8, int8x8x2_t, const int8_t *)

/*
** test_vld2_dup_s16:
** ld2r	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_s16, int16x4x2_t, const int16_t *)

/*
** test_vld2_dup_s32:
** ld2r	{v0\.2s - v1\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_s32, int32x2x2_t, const int32_t *)

/*
** test_vld2_dup_s64:
** ld2r	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_s64, int64x1x2_t, const int64_t *)

/*
** test_vld2_dup_u8:
** ld2r	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_u8, uint8x8x2_t, const uint8_t *)

/*
** test_vld2_dup_u16:
** ld2r	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_u16, uint16x4x2_t, const uint16_t *)

/*
** test_vld2_dup_u32:
** ld2r	{v0\.2s - v1\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_u32, uint32x2x2_t, const uint32_t *)

/*
** test_vld2_dup_u64:
** ld2r	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_u64, uint64x1x2_t, const uint64_t *)

/*
** test_vld2_dup_p8:
** ld2r	{v0\.8b - v1\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_p8, poly8x8x2_t, const poly8_t *)

/*
** test_vld2_dup_p16:
** ld2r	{v0\.4h - v1\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_p16, poly16x4x2_t, const poly16_t *)

/*
** test_vld2_dup_p64:
** ld2r	{v0\.1d - v1\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld2_dup_p64, poly64x1x2_t, const poly64_t *)

/*
** test_vld2q_dup_bf16:
** ld2r	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_bf16, bfloat16x8x2_t, const bfloat16_t *)

/*
** test_vld2q_dup_f16:
** ld2r	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_f16, float16x8x2_t, const float16_t *)

/*
** test_vld2q_dup_f32:
** ld2r	{v0\.4s - v1\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_f32, float32x4x2_t, const float32_t *)

/*
** test_vld2q_dup_f64:
** ld2r	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_f64, float64x2x2_t, const float64_t *)

/*
** test_vld2q_dup_mf8:
** ld2r	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_mf8, mfloat8x16x2_t, const mfloat8_t *)

/*
** test_vld2q_dup_s8:
** ld2r	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_s8, int8x16x2_t, const int8_t *)

/*
** test_vld2q_dup_s16:
** ld2r	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_s16, int16x8x2_t, const int16_t *)

/*
** test_vld2q_dup_s32:
** ld2r	{v0\.4s - v1\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_s32, int32x4x2_t, const int32_t *)

/*
** test_vld2q_dup_s64:
** ld2r	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_s64, int64x2x2_t, const int64_t *)

/*
** test_vld2q_dup_u8:
** ld2r	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_u8, uint8x16x2_t, const uint8_t *)

/*
** test_vld2q_dup_u16:
** ld2r	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_u16, uint16x8x2_t, const uint16_t *)

/*
** test_vld2q_dup_u32:
** ld2r	{v0\.4s - v1\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_u32, uint32x4x2_t, const uint32_t *)

/*
** test_vld2q_dup_u64:
** ld2r	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_u64, uint64x2x2_t, const uint64_t *)

/*
** test_vld2q_dup_p8:
** ld2r	{v0\.16b - v1\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_p8, poly8x16x2_t, const poly8_t *)

/*
** test_vld2q_dup_p16:
** ld2r	{v0\.8h - v1\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_p16, poly16x8x2_t, const poly16_t *)

/*
** test_vld2q_dup_p64:
** ld2r	{v0\.2d - v1\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld2q_dup_p64, poly64x2x2_t, const poly64_t *)
