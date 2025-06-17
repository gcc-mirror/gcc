/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" { target aarch64_little_endian } } } */

#include <arm_neon.h>

#define TEST(TYPE, A, B, C, D)			\
  TYPE						\
  test_##TYPE (TYPE a)				\
  {						\
    a.val[A][B] = a.val[C][D];			\
    return a;					\
  }

/*
** test_bfloat16x8x2_t:
**	ins	v1\.h\[6\], v0\.h\[5\]
**	ret
*/
TEST (bfloat16x8x2_t, 1, 6, 0, 5)

/*
** test_float16x8x2_t:
**	ins	v1\.h\[2\], v0\.h\[7\]
**	ret
*/
TEST (float16x8x2_t, 1, 2, 0, 7)

/*
** test_float32x4x2_t:
**	ins	v1\.s\[3\], v0\.s\[1\]
**	ret
*/
TEST (float32x4x2_t, 1, 3, 0, 1)

/*
** test_float64x2x2_t:
**	ins	v1\.d\[0\], v0\.d\[0\]
**	ret
*/
TEST (float64x2x2_t, 1, 0, 0, 0)

/*
** test_int8x16x2_t:
**	ins	v0\.b\[15\], v1\.b\[13\]
**	ret
*/
TEST (int8x16x2_t, 0, 15, 1, 13)

/*
** test_int16x8x2_t:
**	ins	v0\.h\[2\], v1\.h\[7\]
**	ret
*/
TEST (int16x8x2_t, 0, 2, 1, 7)

/*
** test_int32x4x2_t:
**	ins	v0\.s\[3\], v1\.s\[1\]
**	ret
*/
TEST (int32x4x2_t, 0, 3, 1, 1)

/*
** test_int64x2x2_t:
**	ins	v0\.d\[0\], v1\.d\[1\]
**	ret
*/
TEST (int64x2x2_t, 0, 0, 1, 1)

/*
** test_uint8x16x2_t:
**	ins	v1\.b\[13\], v0\.b\[11\]
**	ret
*/
TEST (uint8x16x2_t, 1, 13, 0, 11)

/*
** test_uint16x8x2_t:
**	ins	v1\.h\[6\], v1\.h\[3\]
**	ret
*/
TEST (uint16x8x2_t, 1, 6, 1, 3)

/*
** test_uint32x4x2_t:
**	ins	v1\.s\[3\], v1\.s\[1\]
**	ret
*/
TEST (uint32x4x2_t, 1, 3, 1, 1)

/*
** test_uint64x2x2_t:
**	ins	v1\.d\[0\], v1\.d\[1\]
**	ret
*/
TEST (uint64x2x2_t, 1, 0, 1, 1)

//--------------------------------------------------------------

/*
** test_bfloat16x8x3_t:
**	ins	v2\.h\[3\], v0\.h\[7\]
**	ret
*/
TEST (bfloat16x8x3_t, 2, 3, 0, 7)

/*
** test_float16x8x3_t:
**	ins	v0\.h\[4\], v1\.h\[6\]
**	ret
*/
TEST (float16x8x3_t, 0, 4, 1, 6)

/*
** test_float32x4x3_t:
**	ins	v1\.s\[2\], v2\.s\[1\]
**	ret
*/
TEST (float32x4x3_t, 1, 2, 2, 1)

/*
** test_float64x2x3_t:
**	ins	v1\.d\[0\], v2\.d\[1\]
**	ret
*/
TEST (float64x2x3_t, 1, 0, 2, 1)

/*
** test_int8x16x3_t:
**	ins	v0\.b\[9\], v2\.b\[14\]
**	ret
*/
TEST (int8x16x3_t, 0, 9, 2, 14)

/*
** test_int16x8x3_t:
**	ins	v2\.h\[6\], v1\.h\[3\]
**	ret
*/
TEST (int16x8x3_t, 2, 6, 1, 3)

/*
** test_int32x4x3_t:
**	ins	v1\.s\[3\], v1\.s\[1\]
**	ret
*/
TEST (int32x4x3_t, 1, 3, 1, 1)

/*
** test_int64x2x3_t:
**	ins	v2\.d\[1\], v1\.d\[0\]
**	ret
*/
TEST (int64x2x3_t, 2, 1, 1, 0)

/*
** test_uint8x16x3_t:
**	ins	v1\.b\[10\], v2\.b\[8\]
**	ret
*/
TEST (uint8x16x3_t, 1, 10, 2, 8)

/*
** test_uint16x8x3_t:
**	ins	v2\.h\[5\], v1\.h\[2\]
**	ret
*/
TEST (uint16x8x3_t, 2, 5, 1, 2)

/*
** test_uint32x4x3_t:
**	ins	v2\.s\[3\], v0\.s\[1\]
**	ret
*/
TEST (uint32x4x3_t, 2, 3, 0, 1)

/*
** test_uint64x2x3_t:
**	ins	v1\.d\[0\], v2\.d\[1\]
**	ret
*/
TEST (uint64x2x3_t, 1, 0, 2, 1)

//--------------------------------------------------------------

/*
** test_bfloat16x8x4_t:
**	ins	v2\.h\[5\], v3\.h\[6\]
**	ret
*/
TEST (bfloat16x8x4_t, 2, 5, 3, 6)

/*
** test_float16x8x4_t:
**	ins	v0\.h\[3\], v3\.h\[5\]
**	ret
*/
TEST (float16x8x4_t, 0, 3, 3, 5)

/*
** test_float32x4x4_t:
**	ins	v3\.s\[2\], v2\.s\[1\]
**	ret
*/
TEST (float32x4x4_t, 3, 2, 2, 1)

/*
** test_float64x2x4_t:
**	ins	v1\.d\[1\], v3\.d\[0\]
**	ret
*/
TEST (float64x2x4_t, 1, 1, 3, 0)

/*
** test_int8x16x4_t:
**	ins	v0\.b\[14\], v3\.b\[10\]
**	ret
*/
TEST (int8x16x4_t, 0, 14, 3, 10)

/*
** test_int16x8x4_t:
**	ins	v3\.h\[4\], v1\.h\[6\]
**	ret
*/
TEST (int16x8x4_t, 3, 4, 1, 6)

/*
** test_int32x4x4_t:
**	ins	v1\.s\[3\], v3\.s\[1\]
**	ret
*/
TEST (int32x4x4_t, 1, 3, 3, 1)

/*
** test_int64x2x4_t:
**	ins	v3\.d\[0\], v2\.d\[0\]
**	ret
*/
TEST (int64x2x4_t, 3, 0, 2, 0)

/*
** test_uint8x16x4_t:
**	ins	v3\.b\[13\], v2\.b\[6\]
**	ret
*/
TEST (uint8x16x4_t, 3, 13, 2, 6)

/*
** test_uint16x8x4_t:
**	ins	v3\.h\[2\], v1\.h\[7\]
**	ret
*/
TEST (uint16x8x4_t, 3, 2, 1, 7)

/*
** test_uint32x4x4_t:
**	ins	v0\.s\[3\], v3\.s\[2\]
**	ret
*/
TEST (uint32x4x4_t, 0, 3, 3, 2)

/*
** test_uint64x2x4_t:
**	ins	v1\.d\[0\], v3\.d\[1\]
**	ret
*/
TEST (uint64x2x4_t, 1, 0, 3, 1)
