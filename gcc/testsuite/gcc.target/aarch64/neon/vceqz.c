/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vceqz_u8:
** cmeq	v0\.8b, v0\.8b, #?0
** ret
*/
TEST_UNARY(vceqz_u8, uint8x8_t, uint8x8_t)

/*
** test_vceqz_s8:
** cmeq	v0\.8b, v0\.8b, #?0
** ret
*/
TEST_UNARY (vceqz_s8, uint8x8_t, int8x8_t)

/*
** test_vceqz_p8:
** cmeq	v0\.8b, v0\.8b, #?0
** ret
*/
TEST_UNARY (vceqz_p8, uint8x8_t, poly8x8_t)

/*
** test_vceqz_u16:
** cmeq	v0\.4h, v0\.4h, #?0
** ret
*/
TEST_UNARY (vceqz_u16, uint16x4_t, uint16x4_t)

/*
** test_vceqz_s16:
** cmeq	v0\.4h, v0\.4h, #?0
** ret
*/
TEST_UNARY (vceqz_s16, uint16x4_t, int16x4_t)

/*
** test_vceqz_f16:
** fcmeq	v0\.4h, v0\.4h, #?0(\.0)?
** ret
*/
TEST_UNARY (vceqz_f16, uint16x4_t, float16x4_t)

/*
** test_vceqz_u32:
** cmeq	v0\.2s, v0\.2s, #?0
** ret
*/
TEST_UNARY (vceqz_u32, uint32x2_t, uint32x2_t)

/*
** test_vceqz_s32:
** cmeq	v0\.2s, v0\.2s, #?0
** ret
*/
TEST_UNARY (vceqz_s32, uint32x2_t, int32x2_t)

/*
** test_vceqz_f32:
** fcmeq	v0\.2s, v0\.2s, #?0(\.0)?
** ret
*/
TEST_UNARY (vceqz_f32, uint32x2_t, float32x2_t)

/*
** test_vceqzs_f32:
** fcmeq	s31, s0, #?0(\.0)?
** fmov	w0, s31
** ret
*/
TEST_UNARY (vceqzs_f32, uint32_t, float32_t)

/*
** test_vceqz_u64:
** cmeq	d0, d0, #?0
** ret
*/
TEST_UNARY (vceqz_u64, uint64x1_t, uint64x1_t)

/*
** test_vceqz_s64:
** cmeq	d0, d0, #?0
** ret
*/
TEST_UNARY (vceqz_s64, uint64x1_t, int64x1_t)

/*
** test_vceqz_p64:
** cmeq	d0, d0, #?0
** ret
*/
TEST_UNARY (vceqz_p64, uint64x1_t, poly64x1_t)

/*
** test_vceqz_f64:
** fcmeq	d0, d0, #?0(\.0)?
** ret
*/
TEST_UNARY (vceqz_f64, uint64x1_t, float64x1_t)

/*
** test_vceqzd_s64:
** cmp	x0, #?0
** csetm	x0, eq
** ret
*/
TEST_UNARY (vceqzd_s64, uint64_t, int64_t)

/*
** test_vceqzd_u64:
** cmp	x0, #?0
** csetm	x0, eq
** ret
*/
TEST_UNARY (vceqzd_u64, uint64_t, uint64_t)

/*
** test_vceqzd_f64:
** fcmeq	d31, d0, #?0(\.0)?
** fmov	x0, d31
** ret
*/
TEST_UNARY (vceqzd_f64, uint64_t, float64_t)

/*
** test_vceqzq_u8:
** cmeq	v0\.16b, v0\.16b, #?0
** ret
*/
TEST_UNARY (vceqzq_u8, uint8x16_t, uint8x16_t)

/*
** test_vceqzq_s8:
** cmeq	v0\.16b, v0\.16b, #?0
** ret
*/
TEST_UNARY (vceqzq_s8, uint8x16_t, int8x16_t)

/*
** test_vceqzq_p8:
** cmeq	v0\.16b, v0\.16b, #?0
** ret
*/
TEST_UNARY (vceqzq_p8, uint8x16_t, poly8x16_t)

/*
** test_vceqzq_u16:
** cmeq	v0\.8h, v0\.8h, #?0
** ret
*/
TEST_UNARY (vceqzq_u16, uint16x8_t, uint16x8_t)

/*
** test_vceqzq_s16:
** cmeq	v0\.8h, v0\.8h, #?0
** ret
*/
TEST_UNARY (vceqzq_s16, uint16x8_t, int16x8_t)

/*
** test_vceqzq_f16:
** fcmeq	v0\.8h, v0\.8h, #?0(\.0)?
** ret
*/
TEST_UNARY (vceqzq_f16, uint16x8_t, float16x8_t)

/*
** test_vceqzq_u32:
** cmeq	v0\.4s, v0\.4s, #?0
** ret
*/
TEST_UNARY (vceqzq_u32, uint32x4_t, uint32x4_t)

/*
** test_vceqzq_s32:
** cmeq	v0\.4s, v0\.4s, #?0
** ret
*/
TEST_UNARY (vceqzq_s32, uint32x4_t, int32x4_t)

/*
** test_vceqzq_f32:
** fcmeq	v0\.4s, v0\.4s, #?0(\.0)?
** ret
*/
TEST_UNARY (vceqzq_f32, uint32x4_t, float32x4_t)

/*
** test_vceqzq_u64:
** cmeq	v0\.2d, v0\.2d, #?0
** ret
*/
TEST_UNARY (vceqzq_u64, uint64x2_t, uint64x2_t)

/*
** test_vceqzq_s64:
** cmeq	v0\.2d, v0\.2d, #?0
** ret
*/
TEST_UNARY (vceqzq_s64, uint64x2_t, int64x2_t)

/*
** test_vceqzq_p64:
** cmeq	v0\.2d, v0\.2d, #?0
** ret
*/
TEST_UNARY (vceqzq_p64, uint64x2_t, poly64x2_t)

/*
** test_vceqzq_f64:
** fcmeq	v0\.2d, v0\.2d, #?0(\.0)?
** ret
*/
TEST_UNARY (vceqzq_f64, uint64x2_t, float64x2_t)
