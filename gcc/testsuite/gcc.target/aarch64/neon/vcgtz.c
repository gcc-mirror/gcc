/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vcgtz_s8:
** cmgt	v0\.8b, v0\.8b, #?0
** ret
*/
TEST_UNARY (vcgtz_s8, uint8x8_t, int8x8_t)

/*
** test_vcgtz_s16:
** cmgt	v0\.4h, v0\.4h, #?0
** ret
*/
TEST_UNARY (vcgtz_s16, uint16x4_t, int16x4_t)

/*
** test_vcgtz_f16:
** fcmgt	v0\.4h, v0\.4h, #?0(\.0)?
** ret
*/
TEST_UNARY (vcgtz_f16, uint16x4_t, float16x4_t)

/*
** test_vcgtz_s32:
** cmgt	v0\.2s, v0\.2s, #?0
** ret
*/
TEST_UNARY (vcgtz_s32, uint32x2_t, int32x2_t)

/*
** test_vcgtz_f32:
** fcmgt	v0\.2s, v0\.2s, #?0(\.0)?
** ret
*/
TEST_UNARY (vcgtz_f32, uint32x2_t, float32x2_t)

/*
** test_vcgtzs_f32:
** fcmgt	s[0-9]+, s0, #?0(\.0)?
** [uf]mov	[wx]0, .+
** ret
*/
TEST_UNARY (vcgtzs_f32, uint32_t, float32_t)

/*
** test_vcgtz_s64:
** cmgt	d0, d0, #?0
** ret
*/
TEST_UNARY (vcgtz_s64, uint64x1_t, int64x1_t)

/*
** test_vcgtz_f64:
** fcmgt	d0, d0, #?0(\.0)?
** ret
*/
TEST_UNARY (vcgtz_f64, uint64x1_t, float64x1_t)

/*
** test_vcgtzd_s64:
** cmp	x0, #?0
** csetm	x0, gt
** ret
*/
TEST_UNARY (vcgtzd_s64, uint64_t, int64_t)

/*
** test_vcgtzd_f64:
** fcmgt	d[0-9]+, d0, #?0(\.0)?
** [uf]mov	x0, .+
** ret
*/
TEST_UNARY (vcgtzd_f64, uint64_t, float64_t)

/*
** test_vcgtzq_s8:
** cmgt	v0\.16b, v0\.16b, #?0
** ret
*/
TEST_UNARY (vcgtzq_s8, uint8x16_t, int8x16_t)

/*
** test_vcgtzq_s16:
** cmgt	v0\.8h, v0\.8h, #?0
** ret
*/
TEST_UNARY (vcgtzq_s16, uint16x8_t, int16x8_t)

/*
** test_vcgtzq_f16:
** fcmgt	v0.8h, v0.8h, #?0(\.0)?
** ret
*/
TEST_UNARY (vcgtzq_f16, uint16x8_t, float16x8_t)

/*
** test_vcgtzq_s32:
** cmgt	v0\.4s, v0\.4s, #?0
** ret
*/
TEST_UNARY (vcgtzq_s32, uint32x4_t, int32x4_t)

/*
** test_vcgtzq_f32:
** fcmgt	v0\.4s, v0\.4s, #?0(\.0)?
** ret
*/
TEST_UNARY (vcgtzq_f32, uint32x4_t, float32x4_t)

/*
** test_vcgtzq_s64:
** cmgt	v0\.2d, v0\.2d, #?0
** ret
*/
TEST_UNARY (vcgtzq_s64, uint64x2_t, int64x2_t)

/*
** test_vcgtzq_f64:
** fcmgt	v0\.2d, v0\.2d, #?0(\.0)?
** ret
*/
TEST_UNARY (vcgtzq_f64, uint64x2_t, float64x2_t)
