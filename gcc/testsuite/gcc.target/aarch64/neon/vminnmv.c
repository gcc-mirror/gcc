/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vminnmv_f16:
** fminnmv	h0, v0\.4h
** ret
*/
TEST_UNARY (vminnmv_f16, float16_t, float16x4_t)

/*
** test_vminnmv_f32:
** fminnmp	s0, v0\.2s
** ret
*/
TEST_UNARY (vminnmv_f32, float32_t, float32x2_t)

/*
** test_vminnmvq_f16:
** fminnmv	h0, v0\.8h
** ret
*/
TEST_UNARY (vminnmvq_f16, float16_t, float16x8_t)

/*
** test_vminnmvq_f32:
** fminnmv	s0, v0\.4s
** ret
*/
TEST_UNARY (vminnmvq_f32, float32_t, float32x4_t)

/*
** test_vminnmvq_f64:
** fminnmp	d0, v0\.2d
** ret
*/
TEST_UNARY (vminnmvq_f64, float64_t, float64x2_t)
