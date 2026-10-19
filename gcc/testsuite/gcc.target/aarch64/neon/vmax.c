/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vmax_u8:
** umax	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vmax_u8, uint8x8_t)

/*
** test_vmax_s8:
** smax	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vmax_s8, int8x8_t)

/*
** test_vmax_u16:
** umax	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vmax_u16, uint16x4_t)

/*
** test_vmax_s16:
** smax	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vmax_s16, int16x4_t)

/*
** test_vmax_f16:
** fmax	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vmax_f16, float16x4_t)

/*
** test_vmax_u32:
** umax	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vmax_u32, uint32x2_t)

/*
** test_vmax_s32:
** smax	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vmax_s32, int32x2_t)

/*
** test_vmax_f32:
** fmax	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vmax_f32, float32x2_t)

/*
** test_vmax_f64:
** fmax	d0, (d0, d1|d1, d0)
** ret
*/
TEST_UNIFORM_BINARY (vmax_f64, float64x1_t)

/*
** test_vmaxq_u8:
** umax	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vmaxq_u8, uint8x16_t)

/*
** test_vmaxq_s8:
** smax	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vmaxq_s8, int8x16_t)

/*
** test_vmaxq_u16:
** umax	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vmaxq_u16, uint16x8_t)

/*
** test_vmaxq_s16:
** smax	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vmaxq_s16, int16x8_t)

/*
** test_vmaxq_f16:
** fmax	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vmaxq_f16, float16x8_t)

/*
** test_vmaxq_u32:
** umax	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vmaxq_u32, uint32x4_t)

/*
** test_vmaxq_s32:
** smax	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vmaxq_s32, int32x4_t)

/*
** test_vmaxq_f32:
** fmax	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vmaxq_f32, float32x4_t)

/*
** test_vmaxq_f64:
** fmax	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_UNIFORM_BINARY (vmaxq_f64, float64x2_t)
