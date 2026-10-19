/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vsqrt_f16:
** fsqrt	v0.4h, v0.4h
** ret
*/
TEST_UNIFORM_UNARY (vsqrt_f16, float16x4_t)

/*
** test_vsqrt_f32:
** fsqrt	v0.2s, v0.2s
** ret
*/
TEST_UNIFORM_UNARY (vsqrt_f32, float32x2_t)

/*
** test_vsqrt_f64:
** fsqrt	d0, d0
** ret
*/
TEST_UNIFORM_UNARY (vsqrt_f64, float64x1_t)

/*
** test_vsqrtq_f16:
** fsqrt	v0.8h, v0.8h
** ret
*/
TEST_UNIFORM_UNARY (vsqrtq_f16, float16x8_t)

/*
** test_vsqrtq_f32:
** fsqrt	v0.4s, v0.4s
** ret
*/
TEST_UNIFORM_UNARY (vsqrtq_f32, float32x4_t)

/*
** test_vsqrtq_f64:
** fsqrt	v0.2d, v0.2d
** ret
*/
TEST_UNIFORM_UNARY (vsqrtq_f64, float64x2_t)
