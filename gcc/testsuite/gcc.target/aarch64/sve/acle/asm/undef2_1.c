/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** int8:
**	ret
*/
TEST_UNDEF (int8, svint8x2_t,
	    z0 = svundef2_s8 ())

/*
** uint8:
**	ret
*/
TEST_UNDEF (uint8, svuint8x2_t,
	    z0 = svundef2_u8 ())

/*
** int16:
**	ret
*/
TEST_UNDEF (int16, svint16x2_t,
	    z0 = svundef2_s16 ())

/*
** uint16:
**	ret
*/
TEST_UNDEF (uint16, svuint16x2_t,
	    z0 = svundef2_u16 ())

/*
** float16:
**	ret
*/
TEST_UNDEF (float16, svfloat16x2_t,
	    z0 = svundef2_f16 ())

/*
** mfloat8:
**	ret
*/
TEST_UNDEF (mfloat8, svmfloat8x2_t,
	    z0 = svundef2_mf8 ())

/*
** bfloat16:
**	ret
*/
TEST_UNDEF (bfloat16, svbfloat16x2_t,
	    z0 = svundef2_bf16 ())

/*
** int32:
**	ret
*/
TEST_UNDEF (int32, svint32x2_t,
	    z0 = svundef2_s32 ())

/*
** uint32:
**	ret
*/
TEST_UNDEF (uint32, svuint32x2_t,
	    z0 = svundef2_u32 ())

/*
** float32:
**	ret
*/
TEST_UNDEF (float32, svfloat32x2_t,
	    z0 = svundef2_f32 ())

/*
** int64:
**	ret
*/
TEST_UNDEF (int64, svint64x2_t,
	    z0 = svundef2_s64 ())

/*
** uint64:
**	ret
*/
TEST_UNDEF (uint64, svuint64x2_t,
	    z0 = svundef2_u64 ())

/*
** float64:
**	ret
*/
TEST_UNDEF (float64, svfloat64x2_t,
	    z0 = svundef2_f64 ())

/*
** bools:
**	ret
*/
TEST_UNDEF_B (bools, svboolx2_t,
	      p0 = svundef2_b ())
