/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vsub_u8:
** sub	v0\.8b, v0\.8b, v1\.8b
** ret
*/
TEST_UNIFORM_BINARY (vsub_u8, uint8x8_t)

/*
** test_vsub_s8:
** sub	v0\.8b, v0\.8b, v1\.8b
** ret
*/
TEST_UNIFORM_BINARY (vsub_s8, int8x8_t)

/*
** test_vsub_u16:
** sub	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_UNIFORM_BINARY (vsub_u16, uint16x4_t)

/*
** test_vsub_s16:
** sub	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_UNIFORM_BINARY (vsub_s16, int16x4_t)

/*
** test_vsub_f16:
** fsub	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_UNIFORM_BINARY (vsub_f16, float16x4_t)

/*
** test_vsub_u32:
** sub	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_UNIFORM_BINARY (vsub_u32, uint32x2_t)

/*
** test_vsub_s32:
** sub	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_UNIFORM_BINARY (vsub_s32, int32x2_t)

/*
** test_vsub_f32:
** fsub	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_UNIFORM_BINARY (vsub_f32, float32x2_t)

/*
** test_vsub_u64:
** sub	d0, d0, d1
** ret
*/
TEST_UNIFORM_BINARY (vsub_u64, uint64x1_t)

/*
** test_vsub_s64:
** sub	d0, d0, d1
** ret
*/
TEST_UNIFORM_BINARY (vsub_s64, int64x1_t)

/*
** test_vsub_f64:
** fsub	d0, d0, d1
** ret
*/
TEST_UNIFORM_BINARY (vsub_f64, float64x1_t)

/*
** test_vsubq_u8:
** sub	v0\.16b, v0\.16b, v1\.16b
** ret
*/
TEST_UNIFORM_BINARY (vsubq_u8, uint8x16_t)

/*
** test_vsubq_s8:
** sub	v0\.16b, v0\.16b, v1\.16b
** ret
*/
TEST_UNIFORM_BINARY (vsubq_s8, int8x16_t)

/*
** test_vsubq_u16:
** sub	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_UNIFORM_BINARY (vsubq_u16, uint16x8_t)

/*
** test_vsubq_s16:
** sub	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_UNIFORM_BINARY (vsubq_s16, int16x8_t)

/*
** test_vsubq_f16:
** fsub	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_UNIFORM_BINARY (vsubq_f16, float16x8_t)

/*
** test_vsubq_u32:
** sub	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_UNIFORM_BINARY (vsubq_u32, uint32x4_t)

/*
** test_vsubq_s32:
** sub	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_UNIFORM_BINARY (vsubq_s32, int32x4_t)

/*
** test_vsubq_f32:
** fsub	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_UNIFORM_BINARY (vsubq_f32, float32x4_t)

/*
** test_vsubq_u64:
** sub	v0\.2d, v0\.2d, v1\.2d
** ret
*/
TEST_UNIFORM_BINARY (vsubq_u64, uint64x2_t)

/*
** test_vsubq_s64:
** sub	v0\.2d, v0\.2d, v1\.2d
** ret
*/
TEST_UNIFORM_BINARY (vsubq_s64, int64x2_t)

/*
** test_vsubq_f64:
** fsub	v0\.2d, v0\.2d, v1\.2d
** ret
*/
TEST_UNIFORM_BINARY (vsubq_f64, float64x2_t)

/*
** test_vsubd_u64:
** sub	x0, x0, x1
** ret
*/
TEST_UNIFORM_BINARY (vsubd_u64, uint64_t)

/*
** test_vsubd_s64:
** sub	x0, x0, x1
** ret
*/
TEST_UNIFORM_BINARY (vsubd_s64, int64_t)
