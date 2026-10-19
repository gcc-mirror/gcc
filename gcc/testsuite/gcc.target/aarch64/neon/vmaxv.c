/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vmaxv_u8:
** umaxv	b([0-9]+), v0\.8b
** umov	x0, v\1\.d\[0\]
** ret
*/
TEST_UNARY (vmaxv_u8, uint8_t, uint8x8_t)

/*
** test_vmaxv_s8:
** smaxv	b([0-9]+), v0\.8b
** umov	x0, v\1\.d\[0\]
** ret
*/
TEST_UNARY (vmaxv_s8, int8_t, int8x8_t)

/*
** test_vmaxv_u16:
** umaxv	h([0-9]+), v0\.4h
** umov	x0, v\1\.d\[0\]
** ret
*/
TEST_UNARY (vmaxv_u16, uint16_t, uint16x4_t)

/*
** test_vmaxv_s16:
** smaxv	h([0-9]+), v0\.4h
** umov	x0, v\1\.d\[0\]
** ret
*/
TEST_UNARY (vmaxv_s16, int16_t, int16x4_t)

/*
** test_vmaxv_u32:
** umaxp	v([0-9]+)\.2s, v0\.2s, v0\.2s
** umov	x0, v\1\.d\[0\]
** ret
*/
TEST_UNARY (vmaxv_u32, uint32_t, uint32x2_t)

/*
** test_vmaxv_s32:
** smaxp	v([0-9]+)\.2s, v0\.2s, v0\.2s
** umov	x0, v\1\.d\[0\]
** ret
*/
TEST_UNARY (vmaxv_s32, int32_t, int32x2_t)

/*
** test_vmaxvq_u8:
** umaxv	b([0-9]+), v0\.16b
** umov	w0, v\1\.b\[0\]
** ret
*/
TEST_UNARY (vmaxvq_u8, uint8_t, uint8x16_t)

/*
** test_vmaxvq_s8:
** smaxv	b([0-9]+), v0\.16b
** umov	w0, v\1\.b\[0\]
** ret
*/
TEST_UNARY (vmaxvq_s8, int8_t, int8x16_t)

/*
** test_vmaxvq_u16:
** umaxv	h([0-9]+), v0\.8h
** umov	w0, v\1\.h\[0\]
** ret
*/
TEST_UNARY (vmaxvq_u16, uint16_t, uint16x8_t)

/*
** test_vmaxvq_s16:
** smaxv	h([0-9]+), v0\.8h
** umov	w0, v\1\.h\[0\]
** ret
*/
TEST_UNARY (vmaxvq_s16, int16_t, int16x8_t)

/*
** test_vmaxvq_u32:
** umaxv	(s[0-9]+), v0\.4s
** fmov	w0, \1
** ret
*/
TEST_UNARY (vmaxvq_u32, uint32_t, uint32x4_t)

/*
** test_vmaxvq_s32:
** smaxv	(s[0-9]+), v0\.4s
** fmov	w0, \1
** ret
*/
TEST_UNARY (vmaxvq_s32, int32_t, int32x4_t)

/*
** test_vmaxv_f16:
** fmaxv	h0, v0\.4h
** ret
*/
TEST_UNARY (vmaxv_f16, float16_t, float16x4_t)

/*
** test_vmaxv_f32:
** fmaxp	s0, v0\.2s
** ret
*/
TEST_UNARY (vmaxv_f32, float32_t, float32x2_t)

/*
** test_vmaxvq_f16:
** fmaxv	h0, v0\.8h
** ret
*/
TEST_UNARY (vmaxvq_f16, float16_t, float16x8_t)

/*
** test_vmaxvq_f32:
** fmaxv	s0, v0\.4s
** ret
*/
TEST_UNARY (vmaxvq_f32, float32_t, float32x4_t)

/*
** test_vmaxvq_f64:
** fmaxp	d0, v0\.2d
** ret
*/
TEST_UNARY (vmaxvq_f64, float64_t, float64x2_t)
