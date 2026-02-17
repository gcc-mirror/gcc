/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vcge_u8:
** cmhs	v0\.8b, v0\.8b, v1\.8b
** ret
*/
TEST_BINARY(vcge_u8, uint8x8_t, uint8x8_t, uint8x8_t)

/*
** test_vcge_s8:
** cmge	v0\.8b, v0\.8b, v1\.8b
** ret
*/
TEST_BINARY (vcge_s8, uint8x8_t, int8x8_t, int8x8_t)

/*
** test_vcge_u16:
** cmhs	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_BINARY (vcge_u16, uint16x4_t, uint16x4_t, uint16x4_t)

/*
** test_vcge_s16:
** cmge	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_BINARY (vcge_s16, uint16x4_t, int16x4_t, int16x4_t)

/*
** test_vcge_f16:
** fcmge	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_BINARY (vcge_f16, uint16x4_t, float16x4_t, float16x4_t)

/*
** test_vcge_u32:
** cmhs	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_BINARY (vcge_u32, uint32x2_t, uint32x2_t, uint32x2_t)

/*
** test_vcge_s32:
** cmge	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_BINARY (vcge_s32, uint32x2_t, int32x2_t, int32x2_t)

/*
** test_vcge_f32:
** fcmge	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_BINARY (vcge_f32, uint32x2_t, float32x2_t, float32x2_t)

/*
** test_vcges_f32:
** fcmge	s[0-9]+, s0, s1
** [uf]mov	w0, .+
** ret
*/
TEST_BINARY (vcges_f32, uint32_t, float32_t, float32_t)

/*
** test_vcge_u64:
** cmhs	d0, d0, d1
** ret
*/
TEST_BINARY (vcge_u64, uint64x1_t, uint64x1_t, uint64x1_t)

/*
** test_vcge_s64:
** cmge	d0, d0, d1
** ret
*/
TEST_BINARY (vcge_s64, uint64x1_t, int64x1_t, int64x1_t)

/*
** test_vcge_f64:
** fcmge	d0, d0, d1
** ret
*/
TEST_BINARY (vcge_f64, uint64x1_t, float64x1_t, float64x1_t)

/*
** test_vcged_f64:
** fcmge	d[0-9]+, d0, d1
** [uf]mov	x0, .+
** ret
*/
TEST_BINARY (vcged_f64, uint64_t, float64_t, float64_t)

/*
** test_vcged_s64:
** cmp	x0, x1
** csetm	x0, ge
** ret
*/
TEST_BINARY (vcged_s64, int64_t, int64_t, int64_t)

/*
** test_vcged_u64:
** cmp	x0, x1
** csetm	x0, cs
** ret
*/
TEST_BINARY (vcged_u64, uint64_t, uint64_t, uint64_t)

/*
** test_vcgeq_u8:
** cmhs	v0\.16b, v0\.16b, v1\.16b
** ret
*/
TEST_BINARY (vcgeq_u8, uint8x16_t, uint8x16_t, uint8x16_t)

/*
** test_vcgeq_s8:
** cmge	v0\.16b, v0\.16b, v1\.16b
** ret
*/
TEST_BINARY (vcgeq_s8, uint8x16_t, int8x16_t, int8x16_t)

/*
** test_vcgeq_u16:
** cmhs	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_BINARY (vcgeq_u16, uint16x8_t, uint16x8_t, uint16x8_t)

/*
** test_vcgeq_s16:
** cmge	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_BINARY (vcgeq_s16, uint16x8_t, int16x8_t, int16x8_t)

/*
** test_vcgeq_f16:
** fcmge	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_BINARY (vcgeq_f16, uint16x8_t, float16x8_t, float16x8_t)

/*
** test_vcgeq_u32:
** cmhs	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_BINARY (vcgeq_u32, uint32x4_t, uint32x4_t, uint32x4_t)

/*
** test_vcgeq_s32:
** cmge	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_BINARY (vcgeq_s32, uint32x4_t, int32x4_t, int32x4_t)

/*
** test_vcgeq_f32:
** fcmge	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_BINARY (vcgeq_f32, uint32x4_t, float32x4_t, float32x4_t)

/*
** test_vcgeq_u64:
** cmhs	v0\.2d, v0\.2d, v1\.2d
** ret
*/
TEST_BINARY (vcgeq_u64, uint64x2_t, uint64x2_t, uint64x2_t)

/*
** test_vcgeq_s64:
** cmge	v0\.2d, v0\.2d, v1\.2d
** ret
*/
TEST_BINARY (vcgeq_s64, uint64x2_t, int64x2_t, int64x2_t)

/*
** test_vcgeq_f64:
** fcmge	v0\.2d, v0\.2d, v1\.2d
** ret
*/
TEST_BINARY (vcgeq_f64, uint64x2_t, float64x2_t, float64x2_t)
