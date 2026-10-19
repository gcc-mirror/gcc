/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vaddv_u8:
** addv	b([0-9]+), v0\.8b
** umov	w0, v\1\.b\[0\]
** ret
*/
TEST_UNARY (vaddv_u8, uint8_t, uint8x8_t)

/*
** test_vaddv_s8:
** addv	b([0-9]+), v0\.8b
** umov	w0, v\1\.b\[0\]
** ret
*/
TEST_UNARY (vaddv_s8, int8_t, int8x8_t)

/*
** test_vaddv_u16:
** addv	h([0-9]+), v0\.4h
** umov	w0, v\1\.h\[0\]
** ret
*/
TEST_UNARY (vaddv_u16, uint16_t, uint16x4_t)

/*
** test_vaddv_s16:
** addv	h([0-9]+), v0\.4h
** umov	w0, v\1\.h\[0\]
** ret
*/
TEST_UNARY (vaddv_s16, int16_t, int16x4_t)

/*
** test_vaddv_u32:
** addp	v([0-9]+)\.2s, v0\.2s, v0\.2s
** fmov	w0, s\1
** ret
*/
TEST_UNARY (vaddv_u32, uint32_t, uint32x2_t)

/*
** test_vaddv_s32:
** addp	v([0-9]+)\.2s, v0\.2s, v0\.2s
** fmov	w0, s\1
** ret
*/
TEST_UNARY (vaddv_s32, int32_t, int32x2_t)

/*
** test_vaddvq_u8:
** addv	b([0-9]+), v0\.16b
** umov	w0, v\1\.b\[0\]
** ret
*/
TEST_UNARY (vaddvq_u8, uint8_t, uint8x16_t)

/*
** test_vaddvq_s8:
** addv	b([0-9]+), v0\.16b
** umov	w0, v\1\.b\[0\]
** ret
*/
TEST_UNARY (vaddvq_s8, int8_t, int8x16_t)

/*
** test_vaddvq_u16:
** addv	h([0-9]+), v0\.8h
** umov	w0, v\1\.h\[0\]
** ret
*/
TEST_UNARY (vaddvq_u16, uint16_t, uint16x8_t)

/*
** test_vaddvq_s16:
** addv	h([0-9]+), v0\.8h
** umov	w0, v\1\.h\[0\]
** ret
*/
TEST_UNARY (vaddvq_s16, int16_t, int16x8_t)

/*
** test_vaddvq_u32:
** addv	(s[0-9]+), v0\.4s
** fmov	w0, \1
** ret
*/
TEST_UNARY (vaddvq_u32, uint32_t, uint32x4_t)

/*
** test_vaddvq_s32:
** addv	(s[0-9]+), v0\.4s
** fmov	w0, \1
** ret
*/
TEST_UNARY (vaddvq_s32, int32_t, int32x4_t)

/*
** test_vaddvq_u64:
** addp	(d[0-9]+), v0\.2d
** fmov	x0, \1
** ret
*/
TEST_UNARY (vaddvq_u64, uint64_t, uint64x2_t)

/*
** test_vaddvq_s64:
** addp	(d[0-9]+), v0\.2d
** fmov	x0, \1
** ret
*/
TEST_UNARY (vaddvq_s64, int64_t, int64x2_t)

/*
** test_vaddv_f32:
** faddp	s0, v0\.2s
** ret
*/
TEST_UNARY (vaddv_f32, float32_t, float32x2_t)

/*
** test_vaddvq_f32:
** faddp	(v[0-9]+\.4s), v0\.4s, v0\.4s
** faddp	v0\.4s, \1, \1
** ret
*/
TEST_UNARY (vaddvq_f32, float32_t, float32x4_t)

/*
** test_vaddvq_f64:
** faddp	d0, v0\.2d
** ret
*/
TEST_UNARY (vaddvq_f64, float64_t, float64x2_t)
