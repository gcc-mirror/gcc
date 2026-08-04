/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vabd_s8:
** sabd	v0\.8b, v0\.8b, v1\.8b
** ret
*/
TEST_UNIFORM_BINARY (vabd_s8, int8x8_t)

/*
** test_vabdq_s8:
** sabd	v0\.16b, v0\.16b, v1\.16b
** ret
*/
TEST_UNIFORM_BINARY (vabdq_s8, int8x16_t)

/*
** test_vabd_s16:
** sabd	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_UNIFORM_BINARY (vabd_s16, int16x4_t)

/*
** test_vabdq_s16:
** sabd	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_UNIFORM_BINARY (vabdq_s16, int16x8_t)

/*
** test_vabd_s32:
** sabd	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_UNIFORM_BINARY (vabd_s32, int32x2_t)

/*
** test_vabdq_s32:
** sabd	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_UNIFORM_BINARY (vabdq_s32, int32x4_t)

/*
** test_vabd_u8:
** uabd	v0\.8b, v0\.8b, v1\.8b
** ret
*/
TEST_UNIFORM_BINARY (vabd_u8, uint8x8_t)

/*
** test_vabdq_u8:
** uabd	v0\.16b, v0\.16b, v1\.16b
** ret
*/
TEST_UNIFORM_BINARY (vabdq_u8, uint8x16_t)

/*
** test_vabd_u16:
** uabd	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_UNIFORM_BINARY (vabd_u16, uint16x4_t)

/*
** test_vabdq_u16:
** uabd	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_UNIFORM_BINARY (vabdq_u16, uint16x8_t)

/*
** test_vabd_u32:
** uabd	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_UNIFORM_BINARY (vabd_u32, uint32x2_t)

/*
** test_vabdq_u32:
** uabd	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_UNIFORM_BINARY (vabdq_u32, uint32x4_t)

/*
** test_vabd_f16:
** fabd	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_UNIFORM_BINARY (vabd_f16, float16x4_t)

/*
** test_vabdq_f16:
** fabd	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_UNIFORM_BINARY (vabdq_f16, float16x8_t)

/*
** test_vabd_f32:
** fabd	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_UNIFORM_BINARY (vabd_f32, float32x2_t)

/*
** test_vabdq_f32:
** fabd	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_UNIFORM_BINARY (vabdq_f32, float32x4_t)

/*
** test_vabd_f64:
** fabd	d0, d0, d1
** ret
*/
TEST_UNIFORM_BINARY (vabd_f64, float64x1_t)

/*
** test_vabdq_f64:
** fabd	v0\.2d, v0\.2d, v1\.2d
** ret
*/
TEST_UNIFORM_BINARY (vabdq_f64, float64x2_t)

/*
** test_vabds_f32:
** fabd	s0, s0, s1
** ret
*/
TEST_UNIFORM_BINARY (vabds_f32, float32_t)

/*
** test_vabdd_f64:
** fabd	d0, d0, d1
** ret
*/
TEST_UNIFORM_BINARY (vabdd_f64, float64_t)
