/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vdiv_f32:
** fdiv	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_UNIFORM_BINARY (vdiv_f32, float32x2_t)

/*
** test_vdivq_f32:
** fdiv	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_UNIFORM_BINARY (vdivq_f32, float32x4_t)

/*
** test_vdiv_f64:
** fdiv	d0, d0, d1
** ret
*/
TEST_UNIFORM_BINARY (vdiv_f64, float64x1_t)

/*
** test_vdivq_f64:
** fdiv	v0\.2d, v0\.2d, v1\.2d
** ret
*/
TEST_UNIFORM_BINARY (vdivq_f64, float64x2_t)

/*
** test_vdiv_f16:
** fdiv	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_UNIFORM_BINARY (vdiv_f16, float16x4_t)

/*
** test_vdivq_f16:
** fdiv	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_UNIFORM_BINARY (vdivq_f16, float16x8_t)
