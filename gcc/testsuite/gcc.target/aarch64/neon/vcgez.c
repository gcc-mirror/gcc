/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vcgez_s8:
** cmge	v0\.8b, v0\.8b, #?0
** ret
*/
TEST_UNARY (vcgez_s8, uint8x8_t, int8x8_t)

/*
** test_vcgez_s16:
** cmge	v0\.4h, v0\.4h, #?0
** ret
*/
TEST_UNARY (vcgez_s16, uint16x4_t, int16x4_t)

/*
** test_vcgez_f16:
** fcmge	v0\.4h, v0\.4h, #?0(\.0)?
** ret
*/
TEST_UNARY (vcgez_f16, uint16x4_t, float16x4_t)

/*
** test_vcgez_s32:
** cmge	v0\.2s, v0\.2s, #?0
** ret
*/
TEST_UNARY (vcgez_s32, uint32x2_t, int32x2_t)

/*
** test_vcgez_f32:
** fcmge	v0\.2s, v0\.2s, #?0(\.0)?
** ret
*/
TEST_UNARY (vcgez_f32, uint32x2_t, float32x2_t)

/*
** test_vcgezs_f32:
** fcmge	s[0-9]+, s0, #?0(\.0)?
** [uf]mov	[wx]0, .+
** ret
*/
TEST_UNARY (vcgezs_f32, uint32_t, float32_t)

/*
** test_vcgez_s64:
** cmge	d0, d0, #?0
** ret
*/
TEST_UNARY (vcgez_s64, uint64x1_t, int64x1_t)

/*
** test_vcgez_f64:
** fcmge	d0, d0, #?0(\.0)?
** ret
*/
TEST_UNARY (vcgez_f64, uint64x1_t, float64x1_t)

/*
** test_vcgezd_s64:
** cmp	x0, 0
** csetm	x0, ge
** ret
*/
TEST_UNARY (vcgezd_s64, uint64_t, int64_t)

/*
** test_vcgezd_f64:
** fcmge	d[0-9]+, d0, #?0(\.0)?
** [uf]mov	x0, .+
** ret
*/
TEST_UNARY (vcgezd_f64, uint64_t, float64_t)

/*
** test_vcgezq_s8:
** cmge	v0\.16b, v0\.16b, #?0
** ret
*/
TEST_UNARY (vcgezq_s8, uint8x16_t, int8x16_t)

/*
** test_vcgezq_s16:
** cmge	v0\.8h, v0\.8h, #?0
** ret
*/
TEST_UNARY (vcgezq_s16, uint16x8_t, int16x8_t)

/*
** test_vcgezq_f16:
** fcmge	v0\.8h, v0\.8h, #?0(\.0)?
** ret
*/
TEST_UNARY (vcgezq_f16, uint16x8_t, float16x8_t)

/*
** test_vcgezq_s32:
** cmge	v0\.4s, v0\.4s, #?0
** ret
*/
TEST_UNARY (vcgezq_s32, uint32x4_t, int32x4_t)

/*
** test_vcgezq_f32:
** fcmge	v0\.4s, v0\.4s, #?0(\.0)?
** ret
*/
TEST_UNARY (vcgezq_f32, uint32x4_t, float32x4_t)

/*
** test_vcgezq_s64:
** cmge	v0\.2d, v0\.2d, #?0
** ret
*/
TEST_UNARY (vcgezq_s64, uint64x2_t, int64x2_t)

/*
** test_vcgezq_f64:
** fcmge	v0\.2d, v0\.2d, #?0(\.0)?
** ret
*/
TEST_UNARY (vcgezq_f64, uint64x2_t, float64x2_t)
