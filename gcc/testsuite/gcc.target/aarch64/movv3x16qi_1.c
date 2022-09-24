/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC aarch64 "arm_neon.h"

#pragma GCC target "+nosimd+fp"

#define TEST_VECTOR(TYPE) \
  TYPE mov_##TYPE (TYPE a, TYPE b) { return b; } \
  TYPE load_##TYPE (TYPE *ptr) { return *ptr; } \
  void store_##TYPE (TYPE *ptr, TYPE a) { *ptr = a; }

TEST_VECTOR (int8x16x3_t)
TEST_VECTOR (int16x8x3_t)
TEST_VECTOR (int32x4x3_t)
TEST_VECTOR (int64x2x3_t)
TEST_VECTOR (float16x8x3_t)
TEST_VECTOR (bfloat16x8x3_t)
TEST_VECTOR (float32x4x3_t)
TEST_VECTOR (float64x2x3_t)

/*
** mov_int8x16x3_t:
**	sub	sp, sp, #48
**	stp	q3, q4, \[sp\]
**	str	q5, \[sp, #?32\]
**	ldp	q0, q1, \[sp\]
**	ldr	q2, \[sp, #?32\]
**	add	sp, sp, #?48
**	ret
*/
/*
** load_int8x16x3_t:
**	ldp	q0, q1, \[x0\]
**	ldr	q2, \[x0, #?32\]
**	ret
*/
/*
** store_int8x16x3_t: { xfail *-*-* }
**	stp	q0, q1, \[x0\]
**	stp	q2, \[x0, #?32\]
**	ret
*/
