/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vcagt_f16:
** facgt	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_BINARY (vcagt_f16, uint16x4_t, float16x4_t, float16x4_t)

/*
** test_vcagt_f32:
** facgt	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_BINARY (vcagt_f32, uint32x2_t, float32x2_t, float32x2_t)

/*
** test_vcagt_f64:
** facgt	d0, d0, d1
** ret
*/
TEST_BINARY (vcagt_f64, uint64x1_t, float64x1_t, float64x1_t)

/*
** test_vcagtq_f16:
** facgt	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_BINARY (vcagtq_f16, uint16x8_t, float16x8_t, float16x8_t)

/*
** test_vcagtq_f32:
** facgt	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_BINARY (vcagtq_f32, uint32x4_t, float32x4_t, float32x4_t)

/*
** test_vcagtq_f64:
** facgt	v0\.2d, v0\.2d, v1\.2d
** ret
*/
TEST_BINARY (vcagtq_f64, uint64x2_t, float64x2_t, float64x2_t)

/*
** test_vcagts_f32:
** facgt	s31, s0, s1
** fmov	w0, s31
** ret
*/
TEST_BINARY (vcagts_f32, uint32_t, float32_t, float32_t)

/*
** test_vcagtd_f64:
** facgt	d31, d0, d1
** fmov	x0, d31
** ret
*/
TEST_BINARY (vcagtd_f64, uint64_t, float64_t, float64_t)
