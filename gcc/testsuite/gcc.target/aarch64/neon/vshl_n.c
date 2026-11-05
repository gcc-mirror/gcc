/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vshl_n_s8_3:
** shl	v0\.8b, v0\.8b, 3
** ret
*/
TEST_UNIFORM_UNARY_N (vshl_n_s8, int8x8_t, 3)

/*
** test_vshlq_n_u16_15:
** shl	v0\.8h, v0\.8h, 15
** ret
*/
TEST_UNIFORM_UNARY_N (vshlq_n_u16, uint16x8_t, 15)

/*
** test_vshlq_n_s64_1:
** add	v0\.2d, v0\.2d, v0\.2d
** ret
*/
TEST_UNIFORM_UNARY_N (vshlq_n_s64, int64x2_t, 1)

/*
** test_vshl_n_s64_7:
** shl	d0, d0, 7
** ret
*/
TEST_UNIFORM_UNARY_N (vshl_n_s64, int64x1_t, 7)

/*
** test_vshlq_n_u32_0:
** ret
*/
TEST_UNIFORM_UNARY_N (vshlq_n_u32, uint32x4_t, 0)

/*
** test_vshld_n_s64_3:
** lsl	x0, x0, 3
** ret
*/
TEST_UNIFORM_UNARY_N (vshld_n_s64, int64_t, 3)
