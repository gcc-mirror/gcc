/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vclez_s8:
** cmle	v0\.8b, v0\.8b, #?0
** ret
*/
TEST_UNARY (vclez_s8, uint8x8_t, int8x8_t)

/*
** test_vclez_s16:
** cmle	v0\.4h, v0\.4h, #?0
** ret
*/
TEST_UNARY (vclez_s16, uint16x4_t, int16x4_t)

/*
** test_vclez_f16:
** fcmle	v0\.4h, v0\.4h, #?0(\.0)?
** ret
*/
TEST_UNARY (vclez_f16, uint16x4_t, float16x4_t)

/*
** test_vclez_s32:
** cmle	v0\.2s, v0\.2s, #?0
** ret
*/
TEST_UNARY (vclez_s32, uint32x2_t, int32x2_t)

/*
** test_vclez_f32:
** fcmle	v0\.2s, v0\.2s, #?0(\.0)?
** ret
*/
TEST_UNARY (vclez_f32, uint32x2_t, float32x2_t)

/*
** test_vclezs_f32:
** fcmle	s[0-9]+, s0, #?0(\.0)?
** fmov	w0, .+
** ret
*/
TEST_UNARY (vclezs_f32, uint32_t, float32_t)

/*
** test_vclez_s64:
** cmle	d0, d0, #?0
** ret
*/
TEST_UNARY (vclez_s64, uint64x1_t, int64x1_t)

/*
** test_vclezd_s64:
** cmp	x0, #?0
** csetm	x0, le
** ret
*/
TEST_UNARY (vclezd_s64, uint64_t, int64_t)

/*
** test_vclezd_f64:
** fcmle	d[0-9]+, d0, #?0(\.0)?
** fmov	x0, .+
** ret
*/
TEST_UNARY (vclezd_f64, uint64_t, float64_t)

/*
** test_vclez_f64:
** fcmle	d0, d0, #?0(\.0)?
** ret
*/
TEST_UNARY (vclez_f64, uint64x1_t, float64x1_t)

/*
** test_vclezq_s8:
** cmle	v0\.16b, v0\.16b, #?0
** ret
*/
TEST_UNARY (vclezq_s8, uint8x16_t, int8x16_t)

/*
** test_vclezq_s16:
** cmle	v0\.8h, v0\.8h, #?0
** ret
*/
TEST_UNARY (vclezq_s16, uint16x8_t, int16x8_t)

/*
** test_vclezq_f16:
** fcmle	v0.8h, v0.8h, #?0(\.0)?
** ret
*/
TEST_UNARY (vclezq_f16, uint16x8_t, float16x8_t)

/*
** test_vclezq_s32:
** cmle	v0\.4s, v0\.4s, #?0
** ret
*/
TEST_UNARY (vclezq_s32, uint32x4_t, int32x4_t)

/*
** test_vclezq_f32:
** fcmle	v0\.4s, v0\.4s, #?0(\.0)?
** ret
*/
TEST_UNARY (vclezq_f32, uint32x4_t, float32x4_t)

/*
** test_vclezq_s64:
** cmle	v0\.2d, v0\.2d, #?0
** ret
*/
TEST_UNARY (vclezq_s64, uint64x2_t, int64x2_t)

/*
** test_vclezq_f64:
** fcmle	v0\.2d, v0\.2d, #?0(\.0)?
** ret
*/
TEST_UNARY (vclezq_f64, uint64x2_t, float64x2_t)
