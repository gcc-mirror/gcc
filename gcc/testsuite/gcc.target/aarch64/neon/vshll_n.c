/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vshll_n_s8_3:
** sshll	v0\.8h, v0\.8b, 3
** ret
*/
TEST_UNARY_N (vshll_n_s8, int16x8_t, int8x8_t, 3)

/*
** test_vshll_n_u16_0:
** uxtl	v0\.4s, v0\.4h
** ret
*/
TEST_UNARY_N (vshll_n_u16, uint32x4_t, uint16x4_t, 0)

/*
** test_vshll_n_u8_8:
** shll	v0\.8h, v0\.8b, 8
** ret
*/
TEST_UNARY_N (vshll_n_u8, uint16x8_t, uint8x8_t, 8)

/*
** test_vshll_high_n_s16_5:
** sshll2	v0\.4s, v0\.8h, 5
** ret
*/
TEST_UNARY_N (vshll_high_n_s16, int32x4_t, int16x8_t, 5)

/*
** test_vshll_high_n_u32_32:
** shll2	v0\.2d, v0\.4s, 32
** ret
*/
TEST_UNARY_N (vshll_high_n_u32, uint64x2_t, uint32x4_t, 32)
