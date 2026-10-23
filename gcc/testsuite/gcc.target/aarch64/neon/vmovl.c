/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vmovl_s8:
** sxtl	v0\.8h, v0\.8b
** ret
*/
TEST_UNARY (vmovl_s8, int16x8_t, int8x8_t)

/*
** test_vmovl_s16:
** sxtl	v0\.4s, v0\.4h
** ret
*/
TEST_UNARY (vmovl_s16, int32x4_t, int16x4_t)

/*
** test_vmovl_s32:
** sxtl	v0\.2d, v0\.2s
** ret
*/
TEST_UNARY (vmovl_s32, int64x2_t, int32x2_t)

/*
** test_vmovl_u8:
** uxtl	v0\.8h, v0\.8b
** ret
*/
TEST_UNARY (vmovl_u8, uint16x8_t, uint8x8_t)

/*
** test_vmovl_u16:
** uxtl	v0\.4s, v0\.4h
** ret
*/
TEST_UNARY (vmovl_u16, uint32x4_t, uint16x4_t)

/*
** test_vmovl_u32:
** uxtl	v0\.2d, v0\.2s
** ret
*/
TEST_UNARY (vmovl_u32, uint64x2_t, uint32x2_t)

/*
** test_vmovl_high_s8:
** sxtl2	v0\.8h, v0\.16b
** ret
*/
TEST_UNARY (vmovl_high_s8, int16x8_t, int8x16_t)

/*
** test_vmovl_high_s16:
** sxtl2	v0\.4s, v0\.8h
** ret
*/
TEST_UNARY (vmovl_high_s16, int32x4_t, int16x8_t)

/*
** test_vmovl_high_s32:
** sxtl2	v0\.2d, v0\.4s
** ret
*/
TEST_UNARY (vmovl_high_s32, int64x2_t, int32x4_t)

/*
** test_vmovl_high_u8:
** uxtl2	v0\.8h, v0\.16b
** ret
*/
TEST_UNARY (vmovl_high_u8, uint16x8_t, uint8x16_t)

/*
** test_vmovl_high_u16:
** uxtl2	v0\.4s, v0\.8h
** ret
*/
TEST_UNARY (vmovl_high_u16, uint32x4_t, uint16x8_t)

/*
** test_vmovl_high_u32:
** uxtl2	v0\.2d, v0\.4s
** ret
*/
TEST_UNARY (vmovl_high_u32, uint64x2_t, uint32x4_t)
