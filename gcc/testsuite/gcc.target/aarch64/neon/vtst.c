/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vtst_u8:
** cmtst	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_BINARY (vtst_u8, uint8x8_t, uint8x8_t, uint8x8_t)

/*
** test_vtst_s8:
** cmtst	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_BINARY (vtst_s8, uint8x8_t, int8x8_t, int8x8_t)

/*
** test_vtst_p8:
** cmtst	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_BINARY (vtst_p8, uint8x8_t, poly8x8_t, poly8x8_t)

/*
** test_vtst_u16:
** cmtst	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_BINARY (vtst_u16, uint16x4_t, uint16x4_t, uint16x4_t)

/*
** test_vtst_s16:
** cmtst	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_BINARY (vtst_s16, uint16x4_t, int16x4_t, int16x4_t)

/*
** test_vtst_u32:
** cmtst	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_BINARY (vtst_u32, uint32x2_t, uint32x2_t, uint32x2_t)

/*
** test_vtst_s32:
** cmtst	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_BINARY (vtst_s32, uint32x2_t, int32x2_t, int32x2_t)

/*
** test_vtst_u64:
** cmtst	d0, (d0, d1|d1, d0)
** ret
*/
TEST_BINARY (vtst_u64, uint64x1_t, uint64x1_t, uint64x1_t)

/*
** test_vtst_s64:
** cmtst	d0, (d0, d1|d1, d0)
** ret
*/
TEST_BINARY (vtst_s64, uint64x1_t, int64x1_t, int64x1_t)

/*
** test_vtst_p64:
** cmtst	d0, (d0, d1|d1, d0)
** ret
*/
TEST_BINARY (vtst_p64, uint64x1_t, poly64x1_t, poly64x1_t)

/*
** test_vtstq_u8:
** cmtst	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_BINARY (vtstq_u8, uint8x16_t, uint8x16_t, uint8x16_t)

/*
** test_vtstq_s8:
** cmtst	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_BINARY (vtstq_s8, uint8x16_t, int8x16_t, int8x16_t)

/*
** test_vtstq_p8:
** cmtst	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_BINARY (vtstq_p8, uint8x16_t, poly8x16_t, poly8x16_t)

/*
** test_vtstq_u16:
** cmtst	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_BINARY (vtstq_u16, uint16x8_t, uint16x8_t, uint16x8_t)

/*
** test_vtstq_s16:
** cmtst	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_BINARY (vtstq_s16, uint16x8_t, int16x8_t, int16x8_t)

/*
** test_vtstq_u32:
** cmtst	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_BINARY (vtstq_u32, uint32x4_t, uint32x4_t, uint32x4_t)

/*
** test_vtstq_s32:
** cmtst	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_BINARY (vtstq_s32, uint32x4_t, int32x4_t, int32x4_t)

/*
** test_vtstq_u64:
** cmtst	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_BINARY (vtstq_u64, uint64x2_t, uint64x2_t, uint64x2_t)

/*
** test_vtstq_s64:
** cmtst	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_BINARY (vtstq_s64, uint64x2_t, int64x2_t, int64x2_t)

/*
** test_vtstq_u64:
** cmtst	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_BINARY (vtstq_p64, uint64x2_t, poly64x2_t, poly64x2_t)

/*
** test_vtstd_s64:
** tst	(x0, x1|x1, x0)
** csetm	x0, ne
** ret
*/
TEST_BINARY (vtstd_s64, uint64_t, int64_t, int64_t)

/*
** test_vtstd_u64:
** tst	(x0, x1|x1, x0)
** csetm	x0, ne
** ret
*/
TEST_BINARY (vtstd_u64, uint64_t, uint64_t, uint64_t)
