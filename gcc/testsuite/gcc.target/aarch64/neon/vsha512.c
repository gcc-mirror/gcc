/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vsha512hq_u64:
** sha512h	q0, q1, v2\.2d
** ret
*/
TEST_UNIFORM_TERNARY (vsha512hq_u64, uint64x2_t)

/*
** test_vsha512h2q_u64:
** sha512h2	q0, q1, v2\.2d
** ret
*/
TEST_UNIFORM_TERNARY (vsha512h2q_u64, uint64x2_t)

/*
** test_vsha512su0q_u64:
** sha512su0	v0\.2d, v1\.2d
** ret
*/
TEST_UNIFORM_BINARY (vsha512su0q_u64, uint64x2_t)

/*
** test_vsha512su1q_u64:
** sha512su1	v0\.2d, v1\.2d, v2\.2d
** ret
*/
TEST_UNIFORM_TERNARY (vsha512su1q_u64, uint64x2_t)
