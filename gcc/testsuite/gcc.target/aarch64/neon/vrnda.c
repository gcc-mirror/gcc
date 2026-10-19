/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vrnda_f16:
** frinta	v0\.4h, v0\.4h
** ret
*/
TEST_UNIFORM_UNARY (vrnda_f16, float16x4_t)

/*
** test_vrnda_f32:
** frinta	v0\.2s, v0\.2s
** ret
*/
TEST_UNIFORM_UNARY (vrnda_f32, float32x2_t)

/*
** test_vrnda_f64:
** frinta	d0, d0
** ret
*/
TEST_UNIFORM_UNARY (vrnda_f64, float64x1_t)

/*
** test_vrndaq_f16:
** frinta	v0\.8h, v0\.8h
** ret
*/
TEST_UNIFORM_UNARY (vrndaq_f16, float16x8_t)

/*
** test_vrndaq_f32:
** frinta	v0\.4s, v0\.4s
** ret
*/
TEST_UNIFORM_UNARY (vrndaq_f32, float32x4_t)
/*
** test_vrndaq_f64:
** frinta	v0\.2d, v0\.2d
** ret
*/
TEST_UNIFORM_UNARY (vrndaq_f64, float64x2_t)
