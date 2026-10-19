/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vrndn_f16:
** frintn	v0\.4h, v0\.4h
** ret
*/
TEST_UNIFORM_UNARY (vrndn_f16, float16x4_t)

/*
** test_vrndn_f32:
** frintn	v0\.2s, v0\.2s
** ret
*/
TEST_UNIFORM_UNARY (vrndn_f32, float32x2_t)

/*
** test_vrndn_f64:
** frintn	d0, d0
** ret
*/
TEST_UNIFORM_UNARY (vrndn_f64, float64x1_t)

/*
** test_vrndnq_f16:
** frintn	v0\.8h, v0\.8h
** ret
*/
TEST_UNIFORM_UNARY (vrndnq_f16, float16x8_t)

/*
** test_vrndnq_f32:
** frintn	v0\.4s, v0\.4s
** ret
*/
TEST_UNIFORM_UNARY (vrndnq_f32, float32x4_t)
/*
** test_vrndnq_f64:
** frintn	v0\.2d, v0\.2d
** ret
*/
TEST_UNIFORM_UNARY (vrndnq_f64, float64x2_t)

/*
** test_vrndns_f32:
** frintn	s0, s0
** ret
*/
TEST_UNIFORM_UNARY (vrndns_f32, float32_t)
