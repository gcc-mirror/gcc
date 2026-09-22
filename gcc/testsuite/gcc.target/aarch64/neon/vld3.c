/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vld3_mf8:
** ld3	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld3_mf8, mfloat8x8x3_t, const mfloat8_t *)

/*
** test_vld3_s8:
** ld3	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld3_s8, int8x8x3_t, const int8_t *)

/*
** test_vld3_u8:
** ld3	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld3_u8, uint8x8x3_t, const uint8_t *)

/*
** test_vld3_s16:
** ld3	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld3_s16, int16x4x3_t, const int16_t *)

/*
** test_vld3_u16:
** ld3	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld3_u16, uint16x4x3_t, const uint16_t *)

/*
** test_vld3_s32:
** ld3	{v0\.2s - v2\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld3_s32, int32x2x3_t, const int32_t *)

/*
** test_vld3_u32:
** ld3	{v0\.2s - v2\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld3_u32, uint32x2x3_t, const uint32_t *)

/*
** test_vld3_s64:
** ld1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld3_s64, int64x1x3_t, const int64_t *)

/*
** test_vld3_u64:
** ld1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld3_u64, uint64x1x3_t, const uint64_t *)

/*
** test_vld3_p8:
** ld3	{v0\.8b - v2\.8b}, \[x0\]
** ret
*/
TEST_UNARY (vld3_p8, poly8x8x3_t, const poly8_t *)

/*
** test_vld3_p16:
** ld3	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld3_p16, poly16x4x3_t, const poly16_t *)

/*
** test_vld3_p64:
** ld1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld3_p64, poly64x1x3_t, const poly64_t *)

/*
** test_vld3_bf16:
** ld3	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld3_bf16, bfloat16x4x3_t, const bfloat16_t *)

/*
** test_vld3_f16:
** ld3	{v0\.4h - v2\.4h}, \[x0\]
** ret
*/
TEST_UNARY (vld3_f16, float16x4x3_t, const float16_t *)

/*
** test_vld3_f32:
** ld3	{v0\.2s - v2\.2s}, \[x0\]
** ret
*/
TEST_UNARY (vld3_f32, float32x2x3_t, const float32_t *)

/*
** test_vld3_f64:
** ld1	{v0\.1d - v2\.1d}, \[x0\]
** ret
*/
TEST_UNARY (vld3_f64, float64x1x3_t, const float64_t *)

/*
** test_vld3q_mf8:
** ld3	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_mf8, mfloat8x16x3_t, const mfloat8_t *)

/*
** test_vld3q_s8:
** ld3	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_s8, int8x16x3_t, const int8_t *)

/*
** test_vld3q_u8:
** ld3	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_u8, uint8x16x3_t, const uint8_t *)

/*
** test_vld3q_s16:
** ld3	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_s16, int16x8x3_t, const int16_t *)

/*
** test_vld3q_u16:
** ld3	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_u16, uint16x8x3_t, const uint16_t *)

/*
** test_vld3q_s32:
** ld3	{v0\.4s - v2\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_s32, int32x4x3_t, const int32_t *)

/*
** test_vld3q_u32:
** ld3	{v0\.4s - v2\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_u32, uint32x4x3_t, const uint32_t *)

/*
** test_vld3q_s64:
** ld3	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_s64, int64x2x3_t, const int64_t *)

/*
** test_vld3q_u64:
** ld3	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_u64, uint64x2x3_t, const uint64_t *)

/*
** test_vld3q_p8:
** ld3	{v0\.16b - v2\.16b}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_p8, poly8x16x3_t, const poly8_t *)

/*
** test_vld3q_p16:
** ld3	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_p16, poly16x8x3_t, const poly16_t *)

/*
** test_vld3q_p64:
** ld3	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_p64, poly64x2x3_t, const poly64_t *)

/*
** test_vld3q_bf16:
** ld3	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_bf16, bfloat16x8x3_t, const bfloat16_t *)

/*
** test_vld3q_f16:
** ld3	{v0\.8h - v2\.8h}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_f16, float16x8x3_t, const float16_t *)

/*
** test_vld3q_f32:
** ld3	{v0\.4s - v2\.4s}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_f32, float32x4x3_t, const float32_t *)

/*
** test_vld3q_f64:
** ld3	{v0\.2d - v2\.2d}, \[x0\]
** ret
*/
TEST_UNARY (vld3q_f64, float64x2x3_t, const float64_t *)
