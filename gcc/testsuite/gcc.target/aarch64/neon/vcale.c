/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vcale_f16:
** facge	v0\.4h, v1\.4h, v0\.4h
** ret
*/
TEST_BINARY (vcale_f16, uint16x4_t, float16x4_t, float16x4_t)

/*
** test_vcale_f32:
** facge	v0\.2s, v1\.2s, v0\.2s
** ret
*/
TEST_BINARY (vcale_f32, uint32x2_t, float32x2_t, float32x2_t)

/*
** test_vcale_f64:
** facge	d0, d1, d0
** ret
*/
TEST_BINARY (vcale_f64, uint64x1_t, float64x1_t, float64x1_t)

/*
** test_vcaleq_f16:
** facge	v0\.8h, v1\.8h, v0\.8h
** ret
*/
TEST_BINARY (vcaleq_f16, uint16x8_t, float16x8_t, float16x8_t)

/*
** test_vcaleq_f32:
** facge	v0\.4s, v1\.4s, v0\.4s
** ret
*/
TEST_BINARY (vcaleq_f32, uint32x4_t, float32x4_t, float32x4_t)

/*
** test_vcaleq_f64:
** facge	v0\.2d, v1\.2d, v0\.2d
** ret
*/
TEST_BINARY (vcaleq_f64, uint64x2_t, float64x2_t, float64x2_t)

/*
** test_vcales_f32:
** facge	s31, s1, s0
** fmov	w0, s31
** ret
*/
TEST_BINARY (vcales_f32, uint32_t, float32_t, float32_t)

/*
** test_vcaled_f64:
** facge	d31, d1, d0
** fmov	x0, d31
** ret
*/
TEST_BINARY (vcaled_f64, uint64_t, float64_t, float64_t)
