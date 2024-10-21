/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC aarch64 "arm_neon.h"

#pragma GCC target "+nosimd+fp"

#define TEST_VECTOR(TYPE) \
  TYPE mov_##TYPE (TYPE a, TYPE b) { return b; } \
  TYPE load_##TYPE (TYPE *ptr) { return *ptr; } \
  void store_##TYPE (TYPE *ptr, TYPE a) { *ptr = a; }

TEST_VECTOR (int8x16x4_t)
TEST_VECTOR (int16x8x4_t)
TEST_VECTOR (int32x4x4_t)
TEST_VECTOR (int64x2x4_t)
TEST_VECTOR (float16x8x4_t)
TEST_VECTOR (bfloat16x8x4_t)
TEST_VECTOR (float32x4x4_t)
TEST_VECTOR (float64x2x4_t)
TEST_VECTOR (mfloat8x16x4_t)

/*
** mov_int8x16x4_t:
**	sub	sp, sp, #64
**	stp	q4, q5, \[sp\]
**	stp	q6, q7, \[sp, #?32\]
**	ldp	q0, q1, \[sp\]
**	ldp	q2, q3, \[sp, #?32\]
**	add	sp, sp, #?64
**	ret
*/
/*
** load_int8x16x4_t:
**	ldp	q0, q1, \[x0\]
**	ldp	q2, q3, \[x0, #?32\]
**	ret
*/
/*
** store_int8x16x4_t: { xfail *-*-* }
**	stp	q0, q1, \[x0\]
**	stp	q2, q3, \[x0, #?32\]
**	ret
*/
