/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vminv_u8:
** uminv	b([0-9]+), v0\.8b
** umov	x0, v\1\.d\[0\]
** ret
*/
TEST_UNARY (vminv_u8, uint8_t, uint8x8_t)

/*
** test_vminv_s8:
** sminv	b([0-9]+), v0\.8b
** umov	x0, v\1\.d\[0\]
** ret
*/
TEST_UNARY (vminv_s8, int8_t, int8x8_t)

/*
** test_vminv_u16:
** uminv	h([0-9]+), v0\.4h
** umov	x0, v\1\.d\[0\]
** ret
*/
TEST_UNARY (vminv_u16, uint16_t, uint16x4_t)

/*
** test_vminv_s16:
** sminv	h([0-9]+), v0\.4h
** umov	x0, v\1\.d\[0\]
** ret
*/
TEST_UNARY (vminv_s16, int16_t, int16x4_t)

/*
** test_vminv_u32:
** uminp	v([0-9]+)\.2s, v0\.2s, v0\.2s
** umov	x0, v\1\.d\[0\]
** ret
*/
TEST_UNARY (vminv_u32, uint32_t, uint32x2_t)

/*
** test_vminv_s32:
** sminp	v([0-9]+)\.2s, v0\.2s, v0\.2s
** umov	x0, v\1\.d\[0\]
** ret
*/
TEST_UNARY (vminv_s32, int32_t, int32x2_t)

/*
** test_vminvq_u8:
** uminv	b([0-9]+), v0\.16b
** umov	w0, v\1\.b\[0\]
** ret
*/
TEST_UNARY (vminvq_u8, uint8_t, uint8x16_t)

/*
** test_vminvq_s8:
** sminv	b([0-9]+), v0\.16b
** umov	w0, v\1\.b\[0\]
** ret
*/
TEST_UNARY (vminvq_s8, int8_t, int8x16_t)

/*
** test_vminvq_u16:
** uminv	h([0-9]+), v0\.8h
** umov	w0, v\1\.h\[0\]
** ret
*/
TEST_UNARY (vminvq_u16, uint16_t, uint16x8_t)

/*
** test_vminvq_s16:
** sminv	h([0-9]+), v0\.8h
** umov	w0, v\1\.h\[0\]
** ret
*/
TEST_UNARY (vminvq_s16, int16_t, int16x8_t)

/*
** test_vminvq_u32:
** uminv	(s[0-9]+), v0\.4s
** fmov	w0, \1
** ret
*/
TEST_UNARY (vminvq_u32, uint32_t, uint32x4_t)

/*
** test_vminvq_s32:
** sminv	(s[0-9]+), v0\.4s
** fmov	w0, \1
** ret
*/
TEST_UNARY (vminvq_s32, int32_t, int32x4_t)

/*
** test_vminv_f16:
** fminv	h0, v0\.4h
** ret
*/
TEST_UNARY (vminv_f16, float16_t, float16x4_t)

/*
** test_vminv_f32:
** fminp	s0, v0\.2s
** ret
*/
TEST_UNARY (vminv_f32, float32_t, float32x2_t)

/*
** test_vminvq_f16:
** fminv	h0, v0\.8h
** ret
*/
TEST_UNARY (vminvq_f16, float16_t, float16x8_t)

/*
** test_vminvq_f32:
** fminv	s0, v0\.4s
** ret
*/
TEST_UNARY (vminvq_f32, float32_t, float32x4_t)

/*
** test_vminvq_f64:
** fminp	d0, v0\.2d
** ret
*/
TEST_UNARY (vminvq_f64, float64_t, float64x2_t)
