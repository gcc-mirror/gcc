/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC aarch64 "arm_neon.h"

#pragma GCC target "+nosimd+fp"

#define TEST_VECTOR(TYPE) \
  TYPE mov_##TYPE (TYPE a, TYPE b) { return b; } \
  TYPE load_##TYPE (TYPE *ptr) { return *ptr; } \
  void store_##TYPE (TYPE *ptr, TYPE a) { *ptr = a; }

TEST_VECTOR (int8x16x2_t)
TEST_VECTOR (int16x8x2_t)
TEST_VECTOR (int32x4x2_t)
TEST_VECTOR (int64x2x2_t)
TEST_VECTOR (float16x8x2_t)
TEST_VECTOR (bfloat16x8x2_t)
TEST_VECTOR (float32x4x2_t)
TEST_VECTOR (float64x2x2_t)
TEST_VECTOR (mfloat8x16x2_t)

/*
** mov_int8x16x2_t:
**	sub	sp, sp, #32
**	stp	q2, q3, \[sp\]
**	ldp	q0, q1, \[sp\]
**	add	sp, sp, #?32
**	ret
*/
/*
** load_int8x16x2_t:
**	ldp	q0, q1, \[x0\]
**	ret
*/
/*
** store_int8x16x2_t: { xfail *-*-* }
**	stp	q0, q1, \[x0\]
**	ret
*/
