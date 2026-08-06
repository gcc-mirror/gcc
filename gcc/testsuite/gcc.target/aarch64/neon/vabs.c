/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vabs_f32:
** fabs	v0\.2s, v0\.2s
** ret
*/
TEST_UNIFORM_UNARY (vabs_f32, float32x2_t)

/*
** test_vabs_f64:
** fabs	d0, d0
** ret
*/
TEST_UNIFORM_UNARY (vabs_f64, float64x1_t)

/*
** test_vabs_s8:
** abs	v0\.8b, v0\.8b
** ret
*/
TEST_UNIFORM_UNARY (vabs_s8, int8x8_t)

/*
** test_vabs_s16:
** abs	v0\.4h, v0\.4h
** ret
*/
TEST_UNIFORM_UNARY (vabs_s16, int16x4_t)

/*
** test_vabs_s32:
** abs	v0\.2s, v0\.2s
** ret
*/
TEST_UNIFORM_UNARY (vabs_s32, int32x2_t)

/* FIXME (Karl Meakin): Performs abs in scalar register, even though it requires
   extra moves:
fmov	x31, d0
cmp	x31, #?0
csneg	x31, x31, x31, ge
fmov	d0, x31
*/

/*
** test_vabs_s64: { xfail *-*-* }
** abs	d0, d0
** ret
*/
TEST_UNIFORM_UNARY (vabs_s64, int64x1_t)

/*
** test_vabsd_s64:
** cmp	x0, #?0
** csneg	x0, x0, x0, ge
** ret
*/
TEST_UNIFORM_UNARY (vabsd_s64, int64_t)

/*
** test_vabsq_f32:
** fabs	v0\.4s, v0\.4s
** ret
*/
TEST_UNIFORM_UNARY (vabsq_f32, float32x4_t)

/*
** test_vabsq_f64:
** fabs	v0\.2d, v0\.2d
** ret
*/
TEST_UNIFORM_UNARY (vabsq_f64, float64x2_t)

/*
** test_vabsq_s8:
** abs	v0\.16b, v0\.16b
** ret
*/
TEST_UNIFORM_UNARY (vabsq_s8, int8x16_t)

/*
** test_vabsq_s16:
** abs	v0\.8h, v0\.8h
** ret
*/
TEST_UNIFORM_UNARY (vabsq_s16, int16x8_t)

/*
** test_vabsq_s32:
** abs	v0\.4s, v0\.4s
** ret
*/
TEST_UNIFORM_UNARY (vabsq_s32, int32x4_t)

/*
** test_vabsq_s64:
** abs	v0\.2d, v0\.2d
** ret
*/
TEST_UNIFORM_UNARY (vabsq_s64, int64x2_t)

/*
** test_vabs_f16:
** fabs	v0\.4h, v0\.4h
** ret
*/
TEST_UNIFORM_UNARY (vabs_f16, float16x4_t)

/*
** test_vabsq_f16:
** fabs	v0\.8h, v0\.8h
** ret
*/
TEST_UNIFORM_UNARY (vabsq_f16, float16x8_t)
