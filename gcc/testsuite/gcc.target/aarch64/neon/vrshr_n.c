/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vrshr_n_s8_3:
** srshr	v0\.8b, v0\.8b, 3
** ret
*/
TEST_UNIFORM_UNARY_N (vrshr_n_s8, int8x8_t, 3)

/*
** test_vrshrq_n_u32_32:
** urshr	v0\.4s, v0\.4s, 32
** ret
*/
TEST_UNIFORM_UNARY_N (vrshrq_n_u32, uint32x4_t, 32)

/*
** test_vrshr_n_s64_64:
** srshr	d0, d0, 64
** ret
*/
TEST_UNIFORM_UNARY_N (vrshr_n_s64, int64x1_t, 64)

/*
** test_vrshr_n_u64_9:
** urshr	d0, d0, 9
** ret
*/
TEST_UNIFORM_UNARY_N (vrshr_n_u64, uint64x1_t, 9)

/*
** test_vrsraq_n_s16_5:
** srsra	v0\.8h, v1\.8h, 5
** ret
*/
TEST_UNIFORM_BINARY_N (vrsraq_n_s16, int16x8_t, 5)

/*
** test_vrsra_n_u64_9:
** ursra	d0, d1, 9
** ret
*/
TEST_UNIFORM_BINARY_N (vrsra_n_u64, uint64x1_t, 9)

/*
** test_vrshrd_n_s64_5:
** fmov	d[0-9]+, x0
** srshr	d[0-9]+, d[0-9]+, 5
** fmov	x0, d[0-9]+
** ret
*/
TEST_UNIFORM_UNARY_N (vrshrd_n_s64, int64_t, 5)

/*
** test_vrsrad_n_u64_5:
** fmov	d[0-9]+, x0
** fmov	d[0-9]+, x1
** ursra	d[0-9]+, d[0-9]+, 5
** fmov	x0, d[0-9]+
** ret
*/
TEST_UNIFORM_BINARY_N (vrsrad_n_u64, uint64_t, 5)
