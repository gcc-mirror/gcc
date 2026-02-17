/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vceq_u8:
** cmeq	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_BINARY(vceq_u8, uint8x8_t, uint8x8_t, uint8x8_t)

/*
** test_vceq_s8:
** cmeq	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_BINARY (vceq_s8, uint8x8_t, int8x8_t, int8x8_t)

/*
** test_vceq_p8:
** cmeq	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_BINARY (vceq_p8, uint8x8_t, poly8x8_t, poly8x8_t)

/*
** test_vceq_u16:
** cmeq	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_BINARY (vceq_u16, uint16x4_t, uint16x4_t, uint16x4_t)

/*
** test_vceq_s16:
** cmeq	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_BINARY (vceq_s16, uint16x4_t, int16x4_t, int16x4_t)

/*
** test_vceq_f16:
** fcmeq	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_BINARY (vceq_f16, uint16x4_t, float16x4_t, float16x4_t)

/*
** test_vceq_u32:
** cmeq	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_BINARY (vceq_u32, uint32x2_t, uint32x2_t, uint32x2_t)

/*
** test_vceq_s32:
** cmeq	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_BINARY (vceq_s32, uint32x2_t, int32x2_t, int32x2_t)

/*
** test_vceq_f32:
** fcmeq	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_BINARY (vceq_f32, uint32x2_t, float32x2_t, float32x2_t)

/*
** test_vceq_u64:
** cmeq	d0, (d0, d1|d1, d0)
** ret
*/
TEST_BINARY (vceq_u64, uint64x1_t, uint64x1_t, uint64x1_t)

/*
** test_vceq_s64:
** cmeq	d0, (d0, d1|d1, d0)
** ret
*/
TEST_BINARY (vceq_s64, uint64x1_t, int64x1_t, int64x1_t)

/*
** test_vceq_p64:
** cmeq	d0, (d0, d1|d1, d0)
** ret
*/
TEST_BINARY (vceq_p64, uint64x1_t, poly64x1_t, poly64x1_t)

/*
** test_vceq_f64:
** fcmeq	d0, (d0, d1|d1, d0)
** ret
*/
TEST_BINARY (vceq_f64, uint64x1_t, float64x1_t, float64x1_t)

/*
** test_vceqq_u8:
** cmeq	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_BINARY (vceqq_u8, uint8x16_t, uint8x16_t, uint8x16_t)

/*
** test_vceqq_s8:
** cmeq	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_BINARY (vceqq_s8, uint8x16_t, int8x16_t, int8x16_t)

/*
** test_vceqq_p8:
** cmeq	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_BINARY (vceqq_p8, uint8x16_t, poly8x16_t, poly8x16_t)

/*
** test_vceqq_u16:
** cmeq	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_BINARY (vceqq_u16, uint16x8_t, uint16x8_t, uint16x8_t)

/*
** test_vceqq_s16:
** cmeq	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_BINARY (vceqq_s16, uint16x8_t, int16x8_t, int16x8_t)

/*
** test_vceqq_f16:
** fcmeq	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_BINARY (vceqq_f16, uint16x8_t, float16x8_t, float16x8_t)

/*
** test_vceqq_u32:
** cmeq	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_BINARY (vceqq_u32, uint32x4_t, uint32x4_t, uint32x4_t)

/*
** test_vceqq_s32:
** cmeq	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_BINARY (vceqq_s32, uint32x4_t, int32x4_t, int32x4_t)

/*
** test_vceqq_f32:
** fcmeq	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_BINARY (vceqq_f32, uint32x4_t, float32x4_t, float32x4_t)

/*
** test_vceqq_u64:
** cmeq	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_BINARY (vceqq_u64, uint64x2_t, uint64x2_t, uint64x2_t)

/*
** test_vceqq_s64:
** cmeq	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_BINARY (vceqq_s64, uint64x2_t, int64x2_t, int64x2_t)

/*
** test_vceqq_p64:
** cmeq	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_BINARY (vceqq_p64, uint64x2_t, poly64x2_t, poly64x2_t)

/*
** test_vceqq_f64:
** fcmeq	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_BINARY (vceqq_f64, uint64x2_t, float64x2_t, float64x2_t)

/*
** test_vceqs_f32:
** fcmeq	s31, s0, s1
** fmov	w0, s31
** ret
*/
TEST_BINARY (vceqs_f32, uint32_t, float32_t, float32_t)

/*
** test_vceqd_s64:
** cmp	x0, x1
** csetm	x0, eq
** ret
*/
TEST_BINARY (vceqd_s64, uint64_t, int64_t, int64_t)

/*
** test_vceqd_u64:
** cmp	x0, x1
** csetm	x0, eq
** ret
*/
TEST_BINARY (vceqd_u64, uint64_t, uint64_t, uint64_t)

/*
** test_vceqd_f64:
** fcmeq	d31, d0, d1
** fmov	x0, d31
** ret
*/
TEST_BINARY (vceqd_f64, uint64_t, float64_t, float64_t)
