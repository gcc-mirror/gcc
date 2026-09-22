/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vld1_dup_f16:
** ld1r	{v0\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_f16, float16x4_t, const float16_t *)

/*
** test_vld1_dup_bf16:
** ld1r	{v0\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_bf16, bfloat16x4_t, const bfloat16_t *)

/*
** test_vld1_dup_f32:
** ld1r	{v0\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_f32, float32x2_t, const float32_t *)

/*
** test_vld1_dup_f64:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_f64, float64x1_t, const float64_t *)

/*
** test_vld1_dup_mf8:
** ld1r	{v0\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_mf8, mfloat8x8_t, const mfloat8_t *)

/*
** test_vld1_dup_s8:
** ld1r	{v0\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_s8, int8x8_t, const int8_t *)

/*
** test_vld1_dup_s16:
** ld1r	{v0\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_s16, int16x4_t, const int16_t *)

/*
** test_vld1_dup_s32:
** ld1r	{v0\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_s32, int32x2_t, const int32_t *)

/*
** test_vld1_dup_s64:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_s64, int64x1_t, const int64_t *)

/*
** test_vld1_dup_u8:
** ld1r	{v0\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_u8, uint8x8_t, const uint8_t *)

/*
** test_vld1_dup_u16:
** ld1r	{v0\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_u16, uint16x4_t, const uint16_t *)

/*
** test_vld1_dup_u32:
** ld1r	{v0\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_u32, uint32x2_t, const uint32_t *)

/*
** test_vld1_dup_u64:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_u64, uint64x1_t, const uint64_t *)

/*
** test_vld1_dup_p8:
** ld1r	{v0\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_p8, poly8x8_t, const poly8_t *)

/*
** test_vld1_dup_p16:
** ld1r	{v0\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_p16, poly16x4_t, const poly16_t *)

/*
** test_vld1_dup_p64:
** ldr	d0, \[x0\]
** ret
*/
TEST_UNARY (vld1_dup_p64, poly64x1_t, const poly64_t *)

/*
** test_vld1q_dup_bf16:
** ld1r	{v0\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_bf16, bfloat16x8_t, const bfloat16_t *)

/*
** test_vld1q_dup_f16:
** ld1r	{v0\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_f16, float16x8_t, const float16_t *)

/*
** test_vld1q_dup_f32:
** ld1r	{v0\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_f32, float32x4_t, const float32_t *)

/*
** test_vld1q_dup_f64:
** ld1r	{v0\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_f64, float64x2_t, const float64_t *)

/*
** test_vld1q_dup_mf8:
** ld1r	{v0\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_mf8, mfloat8x16_t, const mfloat8_t *)

/*
** test_vld1q_dup_s8:
** ld1r	{v0\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_s8, int8x16_t, const int8_t *)

/*
** test_vld1q_dup_s16:
** ld1r	{v0\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_s16, int16x8_t, const int16_t *)

/*
** test_vld1q_dup_s32:
** ld1r	{v0\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_s32, int32x4_t, const int32_t *)

/*
** test_vld1q_dup_s64:
** ld1r	{v0\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_s64, int64x2_t, const int64_t *)

/*
** test_vld1q_dup_u8:
** ld1r	{v0\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_u8, uint8x16_t, const uint8_t *)

/*
** test_vld1q_dup_u16:
** ld1r	{v0\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_u16, uint16x8_t, const uint16_t *)

/*
** test_vld1q_dup_u32:
** ld1r	{v0\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_u32, uint32x4_t, const uint32_t *)

/*
** test_vld1q_dup_u64:
** ld1r	{v0\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_u64, uint64x2_t, const uint64_t *)

/*
** test_vld1q_dup_p8:
** ld1r	{v0\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_p8, poly8x16_t, const poly8_t *)

/*
** test_vld1q_dup_p16:
** ld1r	{v0\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_p16, poly16x8_t, const poly16_t *)

/*
** test_vld1q_dup_p64:
** ld1r	{v0\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld1q_dup_p64, poly64x2_t, const poly64_t *)
