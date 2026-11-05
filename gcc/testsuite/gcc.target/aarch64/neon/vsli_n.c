/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vsli_n_s8_3:
** sli	v0\.8b, v1\.8b, 3
** ret
*/
TEST_UNIFORM_BINARY_N (vsli_n_s8, int8x8_t, 3)

/*
** test_vsliq_n_p64_63:
** sli	v0\.2d, v1\.2d, 63
** ret
*/
TEST_UNIFORM_BINARY_N (vsliq_n_p64, poly64x2_t, 63)

/*
** test_vsli_n_u64_7:
** sli	d0, d1, 7
** ret
*/
TEST_UNIFORM_BINARY_N (vsli_n_u64, uint64x1_t, 7)

/*
** test_vsri_n_p8_3:
** sri	v0\.8b, v1\.8b, 3
** ret
*/
TEST_UNIFORM_BINARY_N (vsri_n_p8, poly8x8_t, 3)

/*
** test_vsriq_n_u64_64:
** sri	v0\.2d, v1\.2d, 64
** ret
*/
TEST_UNIFORM_BINARY_N (vsriq_n_u64, uint64x2_t, 64)

/*
** test_vsri_n_s64_7:
** sri	d0, d1, 7
** ret
*/
TEST_UNIFORM_BINARY_N (vsri_n_s64, int64x1_t, 7)

/*
** test_vslid_n_s64_7:
** fmov	d[0-9]+, x0
** fmov	d[0-9]+, x1
** sli	d[0-9]+, d[0-9]+, 7
** fmov	x0, d[0-9]+
** ret
*/
TEST_UNIFORM_BINARY_N (vslid_n_s64, int64_t, 7)

/*
** test_vsrid_n_u64_7:
** fmov	d[0-9]+, x0
** fmov	d[0-9]+, x1
** sri	d[0-9]+, d[0-9]+, 7
** fmov	x0, d[0-9]+
** ret
*/
TEST_UNIFORM_BINARY_N (vsrid_n_u64, uint64_t, 7)
