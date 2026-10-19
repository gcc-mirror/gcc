/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vminnm_f16:
** fminnm	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vminnm_f16, float16x4_t)

/*
** test_vminnm_f32:
** fminnm	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vminnm_f32, float32x2_t)

/*
** test_vminnm_f64:
** fminnm	d0, (d0, d1|d1, d0)
** ret
*/
TEST_UNIFORM_BINARY (vminnm_f64, float64x1_t)

/*
** test_vminnmq_f16:
** fminnm	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vminnmq_f16, float16x8_t)

/*
** test_vminnmq_f32:
** fminnm	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vminnmq_f32, float32x4_t)

/*
** test_vminnmq_f64:
** fminnm	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_UNIFORM_BINARY (vminnmq_f64, float64x2_t)
