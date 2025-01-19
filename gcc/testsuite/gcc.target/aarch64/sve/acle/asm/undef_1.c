/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** int8:
**	ret
*/
TEST_UNDEF (int8, svint8_t,
	    z0 = svundef_s8 ())

/*
** uint8:
**	ret
*/
TEST_UNDEF (uint8, svuint8_t,
	    z0 = svundef_u8 ())

/*
** int16:
**	ret
*/
TEST_UNDEF (int16, svint16_t,
	    z0 = svundef_s16 ())

/*
** uint16:
**	ret
*/
TEST_UNDEF (uint16, svuint16_t,
	    z0 = svundef_u16 ())

/*
** float16:
**	ret
*/
TEST_UNDEF (float16, svfloat16_t,
	    z0 = svundef_f16 ())

/*
** mfloat8:
**	ret
*/
TEST_UNDEF (mfloat8, svmfloat8_t,
	    z0 = svundef_mf8 ())

/*
** bfloat16:
**	ret
*/
TEST_UNDEF (bfloat16, svbfloat16_t,
	    z0 = svundef_bf16 ())

/*
** int32:
**	ret
*/
TEST_UNDEF (int32, svint32_t,
	    z0 = svundef_s32 ())

/*
** uint32:
**	ret
*/
TEST_UNDEF (uint32, svuint32_t,
	    z0 = svundef_u32 ())

/*
** float32:
**	ret
*/
TEST_UNDEF (float32, svfloat32_t,
	    z0 = svundef_f32 ())

/*
** int64:
**	ret
*/
TEST_UNDEF (int64, svint64_t,
	    z0 = svundef_s64 ())

/*
** uint64:
**	ret
*/
TEST_UNDEF (uint64, svuint64_t,
	    z0 = svundef_u64 ())

/*
** float64:
**	ret
*/
TEST_UNDEF (float64, svfloat64_t,
	    z0 = svundef_f64 ())
