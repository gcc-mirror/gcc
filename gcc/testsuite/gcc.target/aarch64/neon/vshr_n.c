/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vshr_n_s8_3:
** sshr	v0\.8b, v0\.8b, 3
** ret
*/
TEST_UNIFORM_UNARY_N (vshr_n_s8, int8x8_t, 3)

/*
** test_vshrq_n_u16_15:
** ushr	v0\.8h, v0\.8h, 15
** ret
*/
TEST_UNIFORM_UNARY_N (vshrq_n_u16, uint16x8_t, 15)

/*
** test_vshr_n_u64_7:
** ushr	d0, d0, 7
** ret
*/
TEST_UNIFORM_UNARY_N (vshr_n_u64, uint64x1_t, 7)

/*
** test_vshr_n_u8_8:
** movi	v0\.2s, 0
** ret
*/
TEST_UNIFORM_UNARY_N (vshr_n_u8, uint8x8_t, 8)

/*
** test_vshrq_n_u32_32:
** movi	v0\.4s, 0
** ret
*/
TEST_UNIFORM_UNARY_N (vshrq_n_u32, uint32x4_t, 32)

/*
** test_vshr_n_s8_8:
** cmlt	v0\.8b, v0\.8b, #0
** ret
*/
TEST_UNIFORM_UNARY_N (vshr_n_s8, int8x8_t, 8)

/*
** test_vshrq_n_s32_32:
** cmlt	v0\.4s, v0\.4s, #0
** ret
*/
TEST_UNIFORM_UNARY_N (vshrq_n_s32, int32x4_t, 32)

/*
** test_vshrd_n_u64_64:
** mov	w0, 0
** ret
*/
TEST_UNIFORM_UNARY_N (vshrd_n_u64, uint64_t, 64)
