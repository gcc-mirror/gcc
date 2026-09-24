/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vsha1cq_u32:
** fmov	s([0-9]+), w0
** sha1c	q0, s\1, v1\.4s
** ret
*/
TEST_TERNARY (vsha1cq_u32, uint32x4_t, uint32x4_t, uint32_t, uint32x4_t)

/*
** test_vsha1mq_u32:
** fmov	s([0-9]+), w0
** sha1m	q0, s\1, v1\.4s
** ret
*/
TEST_TERNARY (vsha1mq_u32, uint32x4_t, uint32x4_t, uint32_t, uint32x4_t)

/*
** test_vsha1pq_u32:
** fmov	s([0-9]+), w0
** sha1p	q0, s\1, v1\.4s
** ret
*/
TEST_TERNARY (vsha1pq_u32, uint32x4_t, uint32x4_t, uint32_t, uint32x4_t)

/*
** test_vsha1h_u32:
** fmov	s([0-9]+), w0
** sha1h	s\1, s\1
** fmov	w0, s\1
** ret
*/
TEST_UNARY (vsha1h_u32, uint32_t, uint32_t)

/*
** test_vsha1su0q_u32:
** sha1su0	v0\.4s, v1\.4s, v2\.4s
** ret
*/
TEST_UNIFORM_TERNARY (vsha1su0q_u32, uint32x4_t)

/*
** test_vsha1su1q_u32:
** sha1su1	v0\.4s, v1\.4s
** ret
*/
TEST_UNIFORM_BINARY (vsha1su1q_u32, uint32x4_t)
