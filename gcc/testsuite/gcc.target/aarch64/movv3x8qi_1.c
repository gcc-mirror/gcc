/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC aarch64 "arm_neon.h"

#pragma GCC target "+nosimd+fp"

#define TEST_VECTOR(TYPE) \
  TYPE mov_##TYPE (TYPE a, TYPE b) { return b; } \
  TYPE load_##TYPE (TYPE *ptr) { return *ptr; } \
  void store_##TYPE (TYPE *ptr, TYPE a) { *ptr = a; }

TEST_VECTOR (int8x8x3_t)
TEST_VECTOR (int16x4x3_t)
TEST_VECTOR (int32x2x3_t)
TEST_VECTOR (int64x1x3_t)
TEST_VECTOR (float16x4x3_t)
TEST_VECTOR (bfloat16x4x3_t)
TEST_VECTOR (float32x2x3_t)
TEST_VECTOR (float64x1x3_t)

/*
** mov_int8x8x3_t:
**	fmov	d0, d3
**	fmov	d1, d4
**	fmov	d2, d5
**	ret
*/
/*
** load_int8x8x3_t:
**	ldp	d0, d1, \[x0\]
**	ldr	d2, \[x0, #?16\]
**	ret
*/
/*
** store_int8x8x3_t:
**	stp	d0, d1, \[x0\]
**	str	d2, \[x0, #?16\]
**	ret
*/
