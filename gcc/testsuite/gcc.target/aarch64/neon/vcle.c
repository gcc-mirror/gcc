/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vcle_u8:
** cmhs	v0\.8b, v1\.8b, v0\.8b
** ret
*/
TEST_BINARY(vcle_u8, uint8x8_t, uint8x8_t, uint8x8_t)

/*
** test_vcle_s8:
** cmge	v0\.8b, v1\.8b, v0\.8b
** ret
*/
TEST_BINARY (vcle_s8, uint8x8_t, int8x8_t, int8x8_t)

/*
** test_vcle_u16:
** cmhs	v0\.4h, v1\.4h, v0\.4h
** ret
*/
TEST_BINARY (vcle_u16, uint16x4_t, uint16x4_t, uint16x4_t)

/*
** test_vcle_s16:
** cmge	v0\.4h, v1\.4h, v0\.4h
** ret
*/
TEST_BINARY (vcle_s16, uint16x4_t, int16x4_t, int16x4_t)

/*
** test_vcle_f16:
** fcmge	v0\.4h, v1\.4h, v0\.4h
** ret
*/
TEST_BINARY (vcle_f16, uint16x4_t, float16x4_t, float16x4_t)

/*
** test_vcle_u32:
** cmhs	v0\.2s, v1\.2s, v0\.2s
** ret
*/
TEST_BINARY (vcle_u32, uint32x2_t, uint32x2_t, uint32x2_t)

/*
** test_vcle_s32:
** cmge	v0\.2s, v1\.2s, v0\.2s
** ret
*/
TEST_BINARY (vcle_s32, uint32x2_t, int32x2_t, int32x2_t)

/*
** test_vcle_f32:
** fcmge	v0\.2s, v1\.2s, v0\.2s
** ret
*/
TEST_BINARY (vcle_f32, uint32x2_t, float32x2_t, float32x2_t)

/*
** test_vcles_f32:
** fcmge	s[0-9]+, s1, s0
** fmov	[wx]0, .+
** ret
*/
TEST_BINARY (vcles_f32, uint16_t, float32_t, float32_t)

/*
** test_vcle_u64:
** cmhs	d0, d1, d0
** ret
*/
TEST_BINARY (vcle_u64, uint64x1_t, uint64x1_t, uint64x1_t)

/*
** test_vcle_s64:
** cmge	d0, d1, d0
** ret
*/
TEST_BINARY (vcle_s64, uint64x1_t, int64x1_t, int64x1_t)

/*
** test_vcle_f64:
** fcmge	d0, d1, d0
** ret
*/
TEST_BINARY (vcle_f64, uint64x1_t, float64x1_t, float64x1_t)

/*
** test_vcled_s64:
** cmp	x0, x1
** csetm	x0, le
** ret
*/
TEST_BINARY (vcled_s64, uint64_t, int64_t, int64_t)

/*
** test_vcled_u64:
** cmp	x0, x1
** csetm	x0, ls
** ret
*/
TEST_BINARY (vcled_u64, uint64_t, uint64_t, uint64_t)

/*
** test_vcled_f64:
** fcmge	d[0-9]+, d1, d0
** [uf]mov	[wx]0, .+
** ret
*/
TEST_BINARY (vcled_f64, uint64_t, float64_t, float64_t)

/*
** test_vcleq_u8:
** cmhs	v0\.16b, v1\.16b, v0\.16b
** ret
*/
TEST_BINARY (vcleq_u8, uint8x16_t, uint8x16_t, uint8x16_t)

/*
** test_vcleq_s8:
** cmge	v0\.16b, v1\.16b, v0\.16b
** ret
*/
TEST_BINARY (vcleq_s8, uint8x16_t, int8x16_t, int8x16_t)

/*
** test_vcleq_u16:
** cmhs	v0\.8h, v1\.8h, v0\.8h
** ret
*/
TEST_BINARY (vcleq_u16, uint16x8_t, uint16x8_t, uint16x8_t)

/*
** test_vcleq_s16:
** cmge	v0\.8h, v1\.8h, v0\.8h
** ret
*/
TEST_BINARY (vcleq_s16, uint16x8_t, int16x8_t, int16x8_t)

/*
** test_vcleq_f16:
** fcmge	v0.8h, v1.8h, v0.8h
** ret
*/
TEST_BINARY (vcleq_f16, uint16x8_t, float16x8_t, float16x8_t)

/*
** test_vcleq_u32:
** cmhs	v0\.4s, v1\.4s, v0\.4s
** ret
*/
TEST_BINARY (vcleq_u32, uint32x4_t, uint32x4_t, uint32x4_t)

/*
** test_vcleq_s32:
** cmge	v0\.4s, v1\.4s, v0\.4s
** ret
*/
TEST_BINARY (vcleq_s32, uint32x4_t, int32x4_t, int32x4_t)

/*
** test_vcleq_f32:
** fcmge	v0\.4s, v1\.4s, v0\.4s
** ret
*/
TEST_BINARY (vcleq_f32, uint32x4_t, float32x4_t, float32x4_t)

/*
** test_vcleq_u64:
** cmhs	v0\.2d, v1\.2d, v0\.2d
** ret
*/
TEST_BINARY (vcleq_u64, uint64x2_t, uint64x2_t, uint64x2_t)

/*
** test_vcleq_s64:
** cmge	v0\.2d, v1\.2d, v0\.2d
** ret
*/
TEST_BINARY (vcleq_s64, uint64x2_t, int64x2_t, int64x2_t)

/*
** test_vcleq_f64:
** fcmge	v0\.2d, v1\.2d, v0\.2d
** ret
*/
TEST_BINARY (vcleq_f64, uint64x2_t, float64x2_t, float64x2_t)
