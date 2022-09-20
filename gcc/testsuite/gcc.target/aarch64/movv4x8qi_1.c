/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC aarch64 "arm_neon.h"

#pragma GCC target "+nosimd+fp"

#define TEST_VECTOR(TYPE) \
  TYPE mov_##TYPE (TYPE a, TYPE b) { return b; } \
  TYPE load_##TYPE (TYPE *ptr) { return *ptr; } \
  void store_##TYPE (TYPE *ptr, TYPE a) { *ptr = a; }

TEST_VECTOR (int8x8x4_t)
TEST_VECTOR (int16x4x4_t)
TEST_VECTOR (int32x2x4_t)
TEST_VECTOR (int64x1x4_t)
TEST_VECTOR (float16x4x4_t)
TEST_VECTOR (bfloat16x4x4_t)
TEST_VECTOR (float32x2x4_t)
TEST_VECTOR (float64x1x4_t)

/*
** mov_int8x8x4_t:
**	fmov	d0, d4
**	fmov	d1, d5
**	fmov	d2, d6
**	fmov	d3, d7
**	ret
*/
/*
** load_int8x8x4_t:
**	ldp	d0, d1, \[x0\]
**	ldp	d2, d3, \[x0, #?16\]
**	ret
*/
/*
** store_int8x8x4_t:
**	stp	d0, d1, \[x0\]
**	stp	d2, d3, \[x0, #?16\]
**	ret
*/
