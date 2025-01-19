/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** int8:
**	ret
*/
TEST_UNDEF (int8, svint8x3_t,
	    z0 = svundef3_s8 ())

/*
** uint8:
**	ret
*/
TEST_UNDEF (uint8, svuint8x3_t,
	    z0 = svundef3_u8 ())

/*
** int16:
**	ret
*/
TEST_UNDEF (int16, svint16x3_t,
	    z0 = svundef3_s16 ())

/*
** uint16:
**	ret
*/
TEST_UNDEF (uint16, svuint16x3_t,
	    z0 = svundef3_u16 ())

/*
** float16:
**	ret
*/
TEST_UNDEF (float16, svfloat16x3_t,
	    z0 = svundef3_f16 ())

/*
** mfloat8:
**	ret
*/
TEST_UNDEF (mfloat8, svmfloat8x3_t,
	    z0 = svundef3_mf8 ())

/*
** bfloat16:
**	ret
*/
TEST_UNDEF (bfloat16, svbfloat16x3_t,
	    z0 = svundef3_bf16 ())

/*
** int32:
**	ret
*/
TEST_UNDEF (int32, svint32x3_t,
	    z0 = svundef3_s32 ())

/*
** uint32:
**	ret
*/
TEST_UNDEF (uint32, svuint32x3_t,
	    z0 = svundef3_u32 ())

/*
** float32:
**	ret
*/
TEST_UNDEF (float32, svfloat32x3_t,
	    z0 = svundef3_f32 ())

/*
** int64:
**	ret
*/
TEST_UNDEF (int64, svint64x3_t,
	    z0 = svundef3_s64 ())

/*
** uint64:
**	ret
*/
TEST_UNDEF (uint64, svuint64x3_t,
	    z0 = svundef3_u64 ())

/*
** float64:
**	ret
*/
TEST_UNDEF (float64, svfloat64x3_t,
	    z0 = svundef3_f64 ())
