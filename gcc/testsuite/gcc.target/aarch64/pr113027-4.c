/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" { target aarch64_little_endian } } } */

#include <arm_neon.h>

#define TEST(TYPE, A, B)		\
  TYPE					\
  test_##TYPE (TYPE a, TYPE *ptr)	\
  {					\
    a.val[A][B] = ptr->val[0][0];	\
    return a;				\
  }

/*
** test_bfloat16x4x2_t:
**	ld1	\{v1\.h\}\[3\], \[x0\]
**	ret
*/
TEST (bfloat16x4x2_t, 1, 3)

/*
** test_float16x4x2_t:
**	ld1	\{v1\.h\}\[1\], \[x0\]
**	ret
*/
TEST (float16x4x2_t, 1, 1)

/*
** test_float32x2x2_t:
**	ld1	\{v1\.s\}\[0\], \[x0\]
**	ret
*/
TEST (float32x2x2_t, 1, 0)

/*
** test_float64x1x2_t:
**	ldr	d1, \[x0\]
**	ret
*/
TEST (float64x1x2_t, 1, 0)

/*
** test_int8x8x2_t:
**	ld1	\{v0\.b\}\[5\], \[x0\]
**	ret
*/
TEST (int8x8x2_t, 0, 5)

/*
** test_int16x4x2_t:
**	ld1	\{v0\.h\}\[2\], \[x0\]
**	ret
*/
TEST (int16x4x2_t, 0, 2)

/*
** test_int32x2x2_t:
**	ld1	\{v0\.s\}\[0\], \[x0\]
**	ret
*/
TEST (int32x2x2_t, 0, 0)

/*
** test_int64x1x2_t:
**	ldr	d0, \[x0\]
**	ret
*/
TEST (int64x1x2_t, 0, 0)

/*
** test_uint8x8x2_t:
**	ld1	\{v1\.b\}\[6\], \[x0\]
**	ret
*/
TEST (uint8x8x2_t, 1, 6)

/*
** test_uint16x4x2_t:
**	ld1	\{v1\.h\}\[2\], \[x0\]
**	ret
*/
TEST (uint16x4x2_t, 1, 2)

/*
** test_uint32x2x2_t:
**	ld1	\{v1\.s\}\[0\], \[x0\]
**	ret
*/
TEST (uint32x2x2_t, 1, 0)

/*
** test_uint64x1x2_t:
**	ldr	d1, \[x0\]
**	ret
*/
TEST (uint64x1x2_t, 1, 0)

//--------------------------------------------------------------

/*
** test_bfloat16x4x3_t:
**	ld1	\{v2\.h\}\[3\], \[x0\]
**	ret
*/
TEST (bfloat16x4x3_t, 2, 3)

/*
** test_float16x4x3_t:
**	ld1	\{v0\.h\}\[1\], \[x0\]
**	ret
*/
TEST (float16x4x3_t, 0, 1)

/*
** test_float32x2x3_t:
**	ld1	\{v1\.s\}\[0\], \[x0\]
**	ret
*/
TEST (float32x2x3_t, 1, 0)

/*
** test_float64x1x3_t:
**	ldr	d1, \[x0\]
**	ret
*/
TEST (float64x1x3_t, 1, 0)

/*
** test_int8x8x3_t:
**	ld1	\{v0\.b\}\[5\], \[x0\]
**	ret
*/
TEST (int8x8x3_t, 0, 5)

/*
** test_int16x4x3_t:
**	ld1	\{v2\.h\}\[2\], \[x0\]
**	ret
*/
TEST (int16x4x3_t, 2, 2)

/*
** test_int32x2x3_t:
**	ld1	\{v1\.s\}\[0\], \[x0\]
**	ret
*/
TEST (int32x2x3_t, 1, 0)

/*
** test_int64x1x3_t:
**	ldr	d2, \[x0\]
**	ret
*/
TEST (int64x1x3_t, 2, 0)

/*
** test_uint8x8x3_t:
**	ld1	\{v1\.b\}\[6\], \[x0\]
**	ret
*/
TEST (uint8x8x3_t, 1, 6)

/*
** test_uint16x4x3_t:
**	ld1	\{v2\.h\}\[2\], \[x0\]
**	ret
*/
TEST (uint16x4x3_t, 2, 2)

/*
** test_uint32x2x3_t:
**	ld1	\{v2\.s\}\[0\], \[x0\]
**	ret
*/
TEST (uint32x2x3_t, 2, 0)

/*
** test_uint64x1x3_t:
**	ldr	d1, \[x0\]
**	ret
*/
TEST (uint64x1x3_t, 1, 0)

//--------------------------------------------------------------

/*
** test_bfloat16x4x4_t:
**	ld1	\{v2\.h\}\[3\], \[x0\]
**	ret
*/
TEST (bfloat16x4x4_t, 2, 3)

/*
** test_float16x4x4_t:
**	ld1	\{v0\.h\}\[2\], \[x0\]
**	ret
*/
TEST (float16x4x4_t, 0, 2)

/*
** test_float32x2x4_t:
**	ld1	\{v3\.s\}\[0\], \[x0\]
**	ret
*/
TEST (float32x2x4_t, 3, 0)

/*
** test_float64x1x4_t:
**	ldr	d1, \[x0\]
**	ret
*/
TEST (float64x1x4_t, 1, 0)

/*
** test_int8x8x4_t:
**	ld1	\{v0\.b\}\[4\], \[x0\]
**	ret
*/
TEST (int8x8x4_t, 0, 4)

/*
** test_int16x4x4_t:
**	ld1	\{v3\.h\}\[3\], \[x0\]
**	ret
*/
TEST (int16x4x4_t, 3, 3)

/*
** test_int32x2x4_t:
**	ld1	\{v1\.s\}\[0\], \[x0\]
**	ret
*/
TEST (int32x2x4_t, 1, 0)

/*
** test_int64x1x4_t:
**	ldr	d3, \[x0\]
**	ret
*/
TEST (int64x1x4_t, 3, 0)

/*
** test_uint8x8x4_t:
**	ld1	\{v3\.b\}\[6\], \[x0\]
**	ret
*/
TEST (uint8x8x4_t, 3, 6)

/*
** test_uint16x4x4_t:
**	ld1	\{v3\.h\}\[1\], \[x0\]
**	ret
*/
TEST (uint16x4x4_t, 3, 1)

/*
** test_uint32x2x4_t:
**	ld1	\{v0\.s\}\[0\], \[x0\]
**	ret
*/
TEST (uint32x2x4_t, 0, 0)

/*
** test_uint64x1x4_t:
**	ldr	d1, \[x0\]
**	ret
*/
TEST (uint64x1x4_t, 1, 0)
