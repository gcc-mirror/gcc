/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" { target aarch64_little_endian } } } */

#include <arm_neon.h>

#define TEST(TYPE, A, B)			\
  TYPE						\
  test_##TYPE (TYPE a, TYPE *ptr)		\
  {						\
    a.val[A][B] = ptr->val[0][0];		\
    return a;					\
  }

/*
** test_bfloat16x8x2_t:
**	ld1	\{v1\.h\}\[6\], \[x0\]
**	ret
*/
TEST (bfloat16x8x2_t, 1, 6)

/*
** test_float16x8x2_t:
**	ld1	\{v1\.h\}\[2\], \[x0\]
**	ret
*/
TEST (float16x8x2_t, 1, 2)

/*
** test_float32x4x2_t:
**	ld1	\{v1\.s\}\[3\], \[x0\]
**	ret
*/
TEST (float32x4x2_t, 1, 3)

/*
** test_float64x2x2_t:
**	ld1	\{v1\.d\}\[0\], \[x0\]
**	ret
*/
TEST (float64x2x2_t, 1, 0)

/*
** test_int8x16x2_t:
**	ld1	\{v0\.b\}\[15\], \[x0\]
**	ret
*/
TEST (int8x16x2_t, 0, 15)

/*
** test_int16x8x2_t:
**	ld1	\{v0\.h\}\[2\], \[x0\]
**	ret
*/
TEST (int16x8x2_t, 0, 2)

/*
** test_int32x4x2_t:
**	ld1	\{v0\.s\}\[3\], \[x0\]
**	ret
*/
TEST (int32x4x2_t, 0, 3)

/*
** test_int64x2x2_t:
**	ld1	\{v0\.d\}\[0\], \[x0\]
**	ret
*/
TEST (int64x2x2_t, 0, 0)

/*
** test_uint8x16x2_t:
**	ld1	\{v1\.b\}\[13\], \[x0\]
**	ret
*/
TEST (uint8x16x2_t, 1, 13)

/*
** test_uint16x8x2_t:
**	ld1	\{v1\.h\}\[6\], \[x0\]
**	ret
*/
TEST (uint16x8x2_t, 1, 6)

/*
** test_uint32x4x2_t:
**	ld1	\{v1\.s\}\[3\], \[x0\]
**	ret
*/
TEST (uint32x4x2_t, 1, 3)

/*
** test_uint64x2x2_t:
**	ld1	\{v1\.d\}\[0\], \[x0\]
**	ret
*/
TEST (uint64x2x2_t, 1, 0)

//--------------------------------------------------------------

/*
** test_bfloat16x8x3_t:
**	ld1	\{v2\.h\}\[3\], \[x0\]
**	ret
*/
TEST (bfloat16x8x3_t, 2, 3)

/*
** test_float16x8x3_t:
**	ld1	\{v0\.h\}\[4\], \[x0\]
**	ret
*/
TEST (float16x8x3_t, 0, 4)

/*
** test_float32x4x3_t:
**	ld1	\{v1\.s\}\[2\], \[x0\]
**	ret
*/
TEST (float32x4x3_t, 1, 2)

/*
** test_float64x2x3_t:
**	ld1	\{v1\.d\}\[0\], \[x0\]
**	ret
*/
TEST (float64x2x3_t, 1, 0)

/*
** test_int8x16x3_t:
**	ld1	\{v0\.b\}\[9\], \[x0\]
**	ret
*/
TEST (int8x16x3_t, 0, 9)

/*
** test_int16x8x3_t:
**	ld1	\{v2\.h\}\[6\], \[x0\]
**	ret
*/
TEST (int16x8x3_t, 2, 6)

/*
** test_int32x4x3_t:
**	ld1	\{v1\.s\}\[3\], \[x0\]
**	ret
*/
TEST (int32x4x3_t, 1, 3)

/*
** test_int64x2x3_t:
**	ld1	\{v2\.d\}\[1\], \[x0\]
**	ret
*/
TEST (int64x2x3_t, 2, 1)

/*
** test_uint8x16x3_t:
**	ld1	\{v1\.b\}\[10\], \[x0\]
**	ret
*/
TEST (uint8x16x3_t, 1, 10)

/*
** test_uint16x8x3_t:
**	ld1	\{v2\.h\}\[5\], \[x0\]
**	ret
*/
TEST (uint16x8x3_t, 2, 5)

/*
** test_uint32x4x3_t:
**	ld1	\{v2\.s\}\[3\], \[x0\]
**	ret
*/
TEST (uint32x4x3_t, 2, 3)

/*
** test_uint64x2x3_t:
**	ld1	\{v1\.d\}\[0\], \[x0\]
**	ret
*/
TEST (uint64x2x3_t, 1, 0)

//--------------------------------------------------------------

/*
** test_bfloat16x8x4_t:
**	ld1	\{v2\.h\}\[5\], \[x0\]
**	ret
*/
TEST (bfloat16x8x4_t, 2, 5)

/*
** test_float16x8x4_t:
**	ld1	\{v0\.h\}\[3\], \[x0\]
**	ret
*/
TEST (float16x8x4_t, 0, 3)

/*
** test_float32x4x4_t:
**	ld1	\{v3\.s\}\[2\], \[x0\]
**	ret
*/
TEST (float32x4x4_t, 3, 2)

/*
** test_float64x2x4_t:
**	ld1	\{v1\.d\}\[1\], \[x0\]
**	ret
*/
TEST (float64x2x4_t, 1, 1)

/*
** test_int8x16x4_t:
**	ld1	\{v0\.b\}\[14\], \[x0\]
**	ret
*/
TEST (int8x16x4_t, 0, 14)

/*
** test_int16x8x4_t:
**	ld1	\{v3\.h\}\[4\], \[x0\]
**	ret
*/
TEST (int16x8x4_t, 3, 4)

/*
** test_int32x4x4_t:
**	ld1	\{v1\.s\}\[3\], \[x0\]
**	ret
*/
TEST (int32x4x4_t, 1, 3)

/*
** test_int64x2x4_t:
**	ld1	\{v3\.d\}\[0\], \[x0\]
**	ret
*/
TEST (int64x2x4_t, 3, 0)

/*
** test_uint8x16x4_t:
**	ld1	\{v3\.b\}\[13\], \[x0\]
**	ret
*/
TEST (uint8x16x4_t, 3, 13)

/*
** test_uint16x8x4_t:
**	ld1	\{v3\.h\}\[2\], \[x0\]
**	ret
*/
TEST (uint16x8x4_t, 3, 2)

/*
** test_uint32x4x4_t:
**	ld1	\{v0\.s\}\[3\], \[x0\]
**	ret
*/
TEST (uint32x4x4_t, 0, 3)

/*
** test_uint64x2x4_t:
**	ld1	\{v1\.d\}\[0\], \[x0\]
**	ret
*/
TEST (uint64x2x4_t, 1, 0)
