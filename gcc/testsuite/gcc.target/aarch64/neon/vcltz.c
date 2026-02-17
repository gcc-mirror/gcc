/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vcltz_s8:
** cmlt	v0\.8b, v0\.8b, #?0
** ret
*/
TEST_UNARY (vcltz_s8, uint8x8_t, int8x8_t)

/*
** test_vcltz_s16:
** cmlt	v0\.4h, v0\.4h, #?0
** ret
*/
TEST_UNARY (vcltz_s16, uint16x4_t, int16x4_t)

/*
** test_vcltz_f16:
** fcmlt	v0\.4h, v0\.4h, #?0(\.0)?
** ret
*/
TEST_UNARY (vcltz_f16, uint16x4_t, float16x4_t)

/*
** test_vcltz_s32:
** cmlt	v0\.2s, v0\.2s, #?0
** ret
*/
TEST_UNARY (vcltz_s32, uint32x2_t, int32x2_t)

/*
** test_vcltz_f32:
** fcmlt	v0\.2s, v0\.2s, #?0(\.0)?
** ret
*/
TEST_UNARY (vcltz_f32, uint32x2_t, float32x2_t)

/*
** test_vcltzs_f32:
** fcmlt	s[0-9]+, s0, #?0(\.0)?
** [uf]mov	[wx]0, .+
** ret
*/
TEST_UNARY (vcltzs_f32, uint32_t, float32_t)

/*
** test_vcltz_s64:
** sshr	d0, d0, #?63
** ret
*/
TEST_UNARY (vcltz_s64, uint64x1_t, int64x1_t)

/*
** test_vcltz_f64:
** fcmlt	d0, d0, #?0(\.0)?
** ret
*/
TEST_UNARY (vcltz_f64, uint64x1_t, float64x1_t)

/*
** test_vcltzd_s64:
** asr	x0, x0, #?63
** ret
*/
TEST_UNARY (vcltzd_s64, uint64_t, int64_t)

/*
** test_vcltzd_f64:
** fcmlt	d[0-9]+, d0, #?0(\.0)?
** [uf]mov	x0, .+
** ret
*/
TEST_UNARY (vcltzd_f64, uint64_t, float64_t)

/*
** test_vcltzq_s8:
** cmlt	v0\.16b, v0\.16b, #?0
** ret
*/
TEST_UNARY (vcltzq_s8, uint8x16_t, int8x16_t)

/*
** test_vcltzq_s16:
** cmlt	v0\.8h, v0\.8h, #?0
** ret
*/
TEST_UNARY (vcltzq_s16, uint16x8_t, int16x8_t)

/*
** test_vcltzq_f16:
** fcmlt	v0.8h, v0.8h, #?0(\.0)?
** ret
*/
TEST_UNARY (vcltzq_f16, uint16x8_t, float16x8_t)

/*
** test_vcltzq_s32:
** cmlt	v0\.4s, v0\.4s, #?0
** ret
*/
TEST_UNARY (vcltzq_s32, uint32x4_t, int32x4_t)

/*
** test_vcltzq_f32:
** fcmlt	v0\.4s, v0\.4s, #?0(\.0)?
** ret
*/
TEST_UNARY (vcltzq_f32, uint32x4_t, float32x4_t)

/*
** test_vcltzq_s64:
** cmlt	v0\.2d, v0\.2d, #?0
** ret
*/
TEST_UNARY (vcltzq_s64, uint64x2_t, int64x2_t)

/*
** test_vcltzq_f64:
** fcmlt	v0\.2d, v0\.2d, #?0(\.0)?
** ret
*/
TEST_UNARY (vcltzq_f64, uint64x2_t, float64x2_t)
