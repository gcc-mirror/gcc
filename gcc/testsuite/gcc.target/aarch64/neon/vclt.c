/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vclt_u8:
** cmhi	v0\.8b, v1\.8b, v0\.8b
** ret
*/
TEST_BINARY(vclt_u8, uint8x8_t, uint8x8_t, uint8x8_t)

/*
** test_vclt_s8:
** cmgt	v0\.8b, v1\.8b, v0\.8b
** ret
*/
TEST_BINARY (vclt_s8, uint8x8_t, int8x8_t, int8x8_t)

/*
** test_vclt_u16:
** cmhi	v0\.4h, v1\.4h, v0\.4h
** ret
*/
TEST_BINARY (vclt_u16, uint16x4_t, uint16x4_t, uint16x4_t)

/*
** test_vclt_s16:
** cmgt	v0\.4h, v1\.4h, v0\.4h
** ret
*/
TEST_BINARY (vclt_s16, uint16x4_t, int16x4_t, int16x4_t)

/*
** test_vclt_f16:
** fcmgt	v0\.4h, v1\.4h, v0\.4h
** ret
*/
TEST_BINARY (vclt_f16, uint16x4_t, float16x4_t, float16x4_t)

/*
** test_vclt_u32:
** cmhi	v0\.2s, v1\.2s, v0\.2s
** ret
*/
TEST_BINARY (vclt_u32, uint32x2_t, uint32x2_t, uint32x2_t)

/*
** test_vclt_s32:
** cmgt	v0\.2s, v1\.2s, v0\.2s
** ret
*/
TEST_BINARY (vclt_s32, uint32x2_t, int32x2_t, int32x2_t)

/*
** test_vclt_f32:
** fcmgt	v0\.2s, v1\.2s, v0\.2s
** ret
*/
TEST_BINARY (vclt_f32, uint32x2_t, float32x2_t, float32x2_t)

/*
** test_vclts_f32:
** fcmgt	s[0-9]+, s1, s0
** [uf]mov	w0, .+
** ret
*/
TEST_BINARY (vclts_f32, uint32_t, float32_t, float32_t)

/*
** test_vclt_u64:
** cmhi	d0, d1, d0
** ret
*/
TEST_BINARY (vclt_u64, uint64x1_t, uint64x1_t, uint64x1_t)

/*
** test_vclt_s64:
** cmgt	d0, d1, d0
** ret
*/
TEST_BINARY (vclt_s64, uint64x1_t, int64x1_t, int64x1_t)

/*
** test_vclt_f64:
** fcmgt	d0, d1, d0
** ret
*/
TEST_BINARY (vclt_f64, uint64x1_t, float64x1_t, float64x1_t)

/*
** test_vcltd_f64:
** fcmgt	d[0-9]+, d1, d0
** [uf]mov	x0, .+
** ret
*/
TEST_BINARY (vcltd_f64, uint64_t, float64_t, float64_t)

/*
** test_vcltd_s64:
** cmp	x0, x1
** csetm	x0, lt
** ret
*/
TEST_BINARY (vcltd_s64, int64_t, int64_t, int64_t)

/*
** test_vcltd_u64:
** cmp	x0, x1
** csetm	x0, cc
** ret
*/
TEST_BINARY (vcltd_u64, uint64_t, uint64_t, uint64_t)

/*
** test_vcltq_u8:
** cmhi	v0\.16b, v1\.16b, v0\.16b
** ret
*/
TEST_BINARY (vcltq_u8, uint8x16_t, uint8x16_t, uint8x16_t)

/*
** test_vcltq_s8:
** cmgt	v0\.16b, v1\.16b, v0\.16b
** ret
*/
TEST_BINARY (vcltq_s8, uint8x16_t, int8x16_t, int8x16_t)

/*
** test_vcltq_u16:
** cmhi	v0\.8h, v1\.8h, v0\.8h
** ret
*/
TEST_BINARY (vcltq_u16, uint16x8_t, uint16x8_t, uint16x8_t)

/*
** test_vcltq_s16:
** cmgt	v0\.8h, v1\.8h, v0\.8h
** ret
*/
TEST_BINARY (vcltq_s16, uint16x8_t, int16x8_t, int16x8_t)

/*
** test_vcltq_f16:
** fcmgt	v0.8h, v1.8h, v0.8h
** ret
*/
TEST_BINARY (vcltq_f16, uint16x8_t, float16x8_t, float16x8_t)

/*
** test_vcltq_u32:
** cmhi	v0\.4s, v1\.4s, v0\.4s
** ret
*/
TEST_BINARY (vcltq_u32, uint32x4_t, uint32x4_t, uint32x4_t)

/*
** test_vcltq_s32:
** cmgt	v0\.4s, v1\.4s, v0\.4s
** ret
*/
TEST_BINARY (vcltq_s32, uint32x4_t, int32x4_t, int32x4_t)

/*
** test_vcltq_f32:
** fcmgt	v0\.4s, v1\.4s, v0\.4s
** ret
*/
TEST_BINARY (vcltq_f32, uint32x4_t, float32x4_t, float32x4_t)

/*
** test_vcltq_u64:
** cmhi	v0\.2d, v1\.2d, v0\.2d
** ret
*/
TEST_BINARY (vcltq_u64, uint64x2_t, uint64x2_t, uint64x2_t)

/*
** test_vcltq_s64:
** cmgt	v0\.2d, v1\.2d, v0\.2d
** ret
*/
TEST_BINARY (vcltq_s64, uint64x2_t, int64x2_t, int64x2_t)

/*
** test_vcltq_f64:
** fcmgt	v0\.2d, v1\.2d, v0\.2d
** ret
*/
TEST_BINARY (vcltq_f64, uint64x2_t, float64x2_t, float64x2_t)
