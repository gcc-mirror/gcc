/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vmin_u8:
** umin	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vmin_u8, uint8x8_t)

/*
** test_vmin_s8:
** smin	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vmin_s8, int8x8_t)

/*
** test_vmin_u16:
** umin	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vmin_u16, uint16x4_t)

/*
** test_vmin_s16:
** smin	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vmin_s16, int16x4_t)

/*
** test_vmin_f16:
** fmin	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vmin_f16, float16x4_t)

/*
** test_vmin_u32:
** umin	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vmin_u32, uint32x2_t)

/*
** test_vmin_s32:
** smin	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vmin_s32, int32x2_t)

/*
** test_vmin_f32:
** fmin	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vmin_f32, float32x2_t)

/*
** test_vmin_f64:
** fmin	d0, (d0, d1|d1, d0)
** ret
*/
TEST_UNIFORM_BINARY (vmin_f64, float64x1_t)

/*
** test_vminq_u8:
** umin	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vminq_u8, uint8x16_t)

/*
** test_vminq_s8:
** smin	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vminq_s8, int8x16_t)

/*
** test_vminq_u16:
** umin	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vminq_u16, uint16x8_t)

/*
** test_vminq_s16:
** smin	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vminq_s16, int16x8_t)

/*
** test_vminq_f16:
** fmin	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vminq_f16, float16x8_t)

/*
** test_vminq_u32:
** umin	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vminq_u32, uint32x4_t)

/*
** test_vminq_s32:
** smin	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vminq_s32, int32x4_t)

/*
** test_vminq_f32:
** fmin	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vminq_f32, float32x4_t)

/*
** test_vminq_f64:
** fmin	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_UNIFORM_BINARY (vminq_f64, float64x2_t)
