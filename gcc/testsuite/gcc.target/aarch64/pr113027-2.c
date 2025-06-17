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
** test_bfloat16x4x2_t:
**	ins	v1\.h\[3\], v0\.h\[2\]
**	ret
*/
TEST (bfloat16x4x2_t, 1, 3, 0, 2)

/*
** test_float16x4x2_t:
**	ins	v1\.h\[1\], v0\.h\[3\]
**	ret
*/
TEST (float16x4x2_t, 1, 1, 0, 3)

/*
** test_float32x2x2_t:
**	ins	v1\.s\[0\], v0\.s\[1\]
**	ret
*/
TEST (float32x2x2_t, 1, 0, 0, 1)

/*
** test_float64x1x2_t:
**	fmov	d1, d0
**	ret
*/
TEST (float64x1x2_t, 1, 0, 0, 0)

/*
** test_int8x8x2_t:
**	ins	v0\.b\[5\], v1\.b\[7\]
**	ret
*/
TEST (int8x8x2_t, 0, 5, 1, 7)

/*
** test_int16x4x2_t:
**	ins	v0\.h\[2\], v1\.h\[2\]
**	ret
*/
TEST (int16x4x2_t, 0, 2, 1, 2)

/*
** test_int32x2x2_t:
**	ins	v0\.s\[0\], v1\.s\[1\]
**	ret
*/
TEST (int32x2x2_t, 0, 0, 1, 1)

/*
** test_int64x1x2_t:
**	fmov	d0, d1
**	ret
*/
TEST (int64x1x2_t, 0, 0, 1, 0)

/*
** test_uint8x8x2_t:
**	ins	v1\.b\[6\], v0\.b\[3\]
**	ret
*/
TEST (uint8x8x2_t, 1, 6, 0, 3)

/*
** test_uint16x4x2_t:
**	ins	v1\.h\[2\], v1\.h\[0\]
**	ret
*/
TEST (uint16x4x2_t, 1, 2, 1, 0)

/*
** test_uint32x2x2_t:
**	ins	v1\.s\[0\], v1\.s\[1\]
**	ret
*/
TEST (uint32x2x2_t, 1, 0, 1, 1)

/*
** test_uint64x1x2_t:
**	fmov	d1, d0
**	ret
*/
TEST (uint64x1x2_t, 1, 0, 0, 0)

//--------------------------------------------------------------

/*
** test_bfloat16x4x3_t:
**	ins	v2\.h\[3\], v0\.h\[2\]
**	ret
*/
TEST (bfloat16x4x3_t, 2, 3, 0, 2)

/*
** test_float16x4x3_t:
**	ins	v0\.h\[1\], v1\.h\[3\]
**	ret
*/
TEST (float16x4x3_t, 0, 1, 1, 3)

/*
** test_float32x2x3_t:
**	ins	v1\.s\[0\], v2\.s\[1\]
**	ret
*/
TEST (float32x2x3_t, 1, 0, 2, 1)

/*
** test_float64x1x3_t:
**	fmov	d1, d2
**	ret
*/
TEST (float64x1x3_t, 1, 0, 2, 0)

/*
** test_int8x8x3_t:
**	ins	v0\.b\[5\], v2\.b\[6\]
**	ret
*/
TEST (int8x8x3_t, 0, 5, 2, 6)

/*
** test_int16x4x3_t:
**	ins	v2\.h\[2\], v1\.h\[1\]
**	ret
*/
TEST (int16x4x3_t, 2, 2, 1, 1)

/*
** test_int32x2x3_t:
**	ins	v1\.s\[0\], v1\.s\[1\]
**	ret
*/
TEST (int32x2x3_t, 1, 0, 1, 1)

/*
** test_int64x1x3_t:
**	fmov	d2, d1
**	ret
*/
TEST (int64x1x3_t, 2, 0, 1, 0)

/*
** test_uint8x8x3_t:
**	ins	v1\.b\[6\], v2\.b\[7\]
**	ret
*/
TEST (uint8x8x3_t, 1, 6, 2, 7)

/*
** test_uint16x4x3_t:
**	ins	v2\.h\[2\], v1\.h\[3\]
**	ret
*/
TEST (uint16x4x3_t, 2, 2, 1, 3)

/*
** test_uint32x2x3_t:
**	ins	v2\.s\[0\], v0\.s\[1\]
**	ret
*/
TEST (uint32x2x3_t, 2, 0, 0, 1)

/*
** test_uint64x1x3_t:
**	fmov	d1, d2
**	ret
*/
TEST (uint64x1x3_t, 1, 0, 2, 0)

//--------------------------------------------------------------

/*
** test_bfloat16x4x4_t:
**	ins	v2\.h\[3\], v3\.h\[2\]
**	ret
*/
TEST (bfloat16x4x4_t, 2, 3, 3, 2)

/*
** test_float16x4x4_t:
**	ins	v0\.h\[2\], v3\.h\[1\]
**	ret
*/
TEST (float16x4x4_t, 0, 2, 3, 1)

/*
** test_float32x2x4_t:
**	ins	v3\.s\[0\], v2\.s\[1\]
**	ret
*/
TEST (float32x2x4_t, 3, 0, 2, 1)

/*
** test_float64x1x4_t:
**	fmov	d1, d3
**	ret
*/
TEST (float64x1x4_t, 1, 0, 3, 0)

/*
** test_int8x8x4_t:
**	ins	v0\.b\[4\], v3\.b\[7\]
**	ret
*/
TEST (int8x8x4_t, 0, 4, 3, 7)

/*
** test_int16x4x4_t:
**	ins	v3\.h\[3\], v1\.h\[1\]
**	ret
*/
TEST (int16x4x4_t, 3, 3, 1, 1)

/*
** test_int32x2x4_t:
**	ins	v1\.s\[0\], v3\.s\[1\]
**	ret
*/
TEST (int32x2x4_t, 1, 0, 3, 1)

/*
** test_int64x1x4_t:
**	fmov	d3, d1
**	ret
*/
TEST (int64x1x4_t, 3, 0, 1, 0)

/*
** test_uint8x8x4_t:
**	ins	v3\.b\[6\], v2\.b\[4\]
**	ret
*/
TEST (uint8x8x4_t, 3, 6, 2, 4)

/*
** test_uint16x4x4_t:
**	ins	v3\.h\[1\], v1\.h\[3\]
**	ret
*/
TEST (uint16x4x4_t, 3, 1, 1, 3)

/*
** test_uint32x2x4_t:
**	ins	v0\.s\[0\], v3\.s\[1\]
**	ret
*/
TEST (uint32x2x4_t, 0, 0, 3, 1)

/*
** test_uint64x1x4_t:
**	fmov	d1, d3
**	ret
*/
TEST (uint64x1x4_t, 1, 0, 3, 0)
