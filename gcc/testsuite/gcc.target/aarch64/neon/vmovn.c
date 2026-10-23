/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vmovn_s16:
** xtn	v0\.8b, v0\.8h
** ret
*/
TEST_UNARY (vmovn_s16, int8x8_t, int16x8_t)

/*
** test_vmovn_s32:
** xtn	v0\.4h, v0\.4s
** ret
*/
TEST_UNARY (vmovn_s32, int16x4_t, int32x4_t)

/*
** test_vmovn_s64:
** xtn	v0\.2s, v0\.2d
** ret
*/
TEST_UNARY (vmovn_s64, int32x2_t, int64x2_t)

/*
** test_vmovn_u16:
** xtn	v0\.8b, v0\.8h
** ret
*/
TEST_UNARY (vmovn_u16, uint8x8_t, uint16x8_t)

/*
** test_vmovn_u32:
** xtn	v0\.4h, v0\.4s
** ret
*/
TEST_UNARY (vmovn_u32, uint16x4_t, uint32x4_t)

/*
** test_vmovn_u64:
** xtn	v0\.2s, v0\.2d
** ret
*/
TEST_UNARY (vmovn_u64, uint32x2_t, uint64x2_t)

/*
** test_vmovn_high_s16:
** xtn2	v0\.16b, v1\.8h
** ret
*/
TEST_BINARY (vmovn_high_s16, int8x16_t, int8x8_t, int16x8_t)

/*
** test_vmovn_high_s32:
** xtn2	v0\.8h, v1\.4s
** ret
*/
TEST_BINARY (vmovn_high_s32, int16x8_t, int16x4_t, int32x4_t)

/*
** test_vmovn_high_s64:
** xtn2	v0\.4s, v1\.2d
** ret
*/
TEST_BINARY (vmovn_high_s64, int32x4_t, int32x2_t, int64x2_t)

/*
** test_vmovn_high_u16:
** xtn2	v0\.16b, v1\.8h
** ret
*/
TEST_BINARY (vmovn_high_u16, uint8x16_t, uint8x8_t, uint16x8_t)

/*
** test_vmovn_high_u32:
** xtn2	v0\.8h, v1\.4s
** ret
*/
TEST_BINARY (vmovn_high_u32, uint16x8_t, uint16x4_t, uint32x4_t)

/*
** test_vmovn_high_u64:
** xtn2	v0\.4s, v1\.2d
** ret
*/
TEST_BINARY (vmovn_high_u64, uint32x4_t, uint32x2_t, uint64x2_t)
