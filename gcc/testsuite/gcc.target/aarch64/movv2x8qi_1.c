/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC aarch64 "arm_neon.h"

#pragma GCC target "+nosimd+fp"

#define TEST_VECTOR(TYPE) \
  TYPE mov_##TYPE (TYPE a, TYPE b) { return b; } \
  TYPE load_##TYPE (TYPE *ptr) { return *ptr; } \
  void store_##TYPE (TYPE *ptr, TYPE a) { *ptr = a; }

TEST_VECTOR (int8x8x2_t)
TEST_VECTOR (int16x4x2_t)
TEST_VECTOR (int32x2x2_t)
TEST_VECTOR (int64x1x2_t)
TEST_VECTOR (float16x4x2_t)
TEST_VECTOR (bfloat16x4x2_t)
TEST_VECTOR (float32x2x2_t)
TEST_VECTOR (float64x1x2_t)

/*
** mov_int8x8x2_t:
**	fmov	d0, d2
**	fmov	d1, d3
**	ret
*/
/*
** load_int8x8x2_t:
**	ldp	d0, d1, \[x0\]
**	ret
*/
/*
** store_int8x8x2_t:
**	stp	d0, d1, \[x0\]
**	ret
*/
