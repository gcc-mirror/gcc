/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vrndx_f16:
** frintx	v0\.4h, v0\.4h
** ret
*/
TEST_UNIFORM_UNARY (vrndx_f16, float16x4_t)

/*
** test_vrndx_f32:
** frintx	v0\.2s, v0\.2s
** ret
*/
TEST_UNIFORM_UNARY (vrndx_f32, float32x2_t)

/*
** test_vrndx_f64:
** frintx	d0, d0
** ret
*/
TEST_UNIFORM_UNARY (vrndx_f64, float64x1_t)

/*
** test_vrndxq_f16:
** frintx	v0\.8h, v0\.8h
** ret
*/
TEST_UNIFORM_UNARY (vrndxq_f16, float16x8_t)

/*
** test_vrndxq_f32:
** frintx	v0\.4s, v0\.4s
** ret
*/
TEST_UNIFORM_UNARY (vrndxq_f32, float32x4_t)
/*
** test_vrndxq_f64:
** frintx	v0\.2d, v0\.2d
** ret
*/
TEST_UNIFORM_UNARY (vrndxq_f64, float64x2_t)
