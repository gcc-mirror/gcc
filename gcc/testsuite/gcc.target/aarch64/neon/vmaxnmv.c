/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vmaxnmv_f16:
** fmaxnmv	h0, v0\.4h
** ret
*/
TEST_UNARY (vmaxnmv_f16, float16_t, float16x4_t)

/*
** test_vmaxnmv_f32:
** fmaxnmp	s0, v0\.2s
** ret
*/
TEST_UNARY (vmaxnmv_f32, float32_t, float32x2_t)

/*
** test_vmaxnmvq_f16:
** fmaxnmv	h0, v0\.8h
** ret
*/
TEST_UNARY (vmaxnmvq_f16, float16_t, float16x8_t)

/*
** test_vmaxnmvq_f32:
** fmaxnmv	s0, v0\.4s
** ret
*/
TEST_UNARY (vmaxnmvq_f32, float32_t, float32x4_t)

/*
** test_vmaxnmvq_f64:
** fmaxnmp	d0, v0\.2d
** ret
*/
TEST_UNARY (vmaxnmvq_f64, float64_t, float64x2_t)
