/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vsha256hq_u32:
** sha256h	q0, q1, v2\.4s
** ret
*/
TEST_UNIFORM_TERNARY (vsha256hq_u32, uint32x4_t)

/*
** test_vsha256h2q_u32:
** sha256h2	q0, q1, v2\.4s
** ret
*/
TEST_UNIFORM_TERNARY (vsha256h2q_u32, uint32x4_t)

/*
** test_vsha256su0q_u32:
** sha256su0	v0\.4s, v1\.4s
** ret
*/
TEST_UNIFORM_BINARY (vsha256su0q_u32, uint32x4_t)

/*
** test_vsha256su1q_u32:
** sha256su1	v0\.4s, v1\.4s, v2\.4s
** ret
*/
TEST_UNIFORM_TERNARY (vsha256su1q_u32, uint32x4_t)
