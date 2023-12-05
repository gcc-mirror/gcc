/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** cvt_z0_z4:
**	fcvtzs	{z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_DUAL_XN (cvt_z0_z4, svint32x2_t, svfloat32x2_t, z0,
	      svcvt_s32_f32_x2 (z4),
	      svcvt_s32 (z4))

/*
** cvt_z4_z0:
**	fcvtzs	{z4\.s - z5\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_DUAL_XN (cvt_z4_z0, svfloat32x2_t, svint32x2_t, z4,
	      svcvt_s32_f32_x2 (z0),
	      svcvt_s32 (z0))

/*
** cvt_z18_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvtzs	{z18\.s - z19\.s}, [^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z18_z23, svint32x2_t, svfloat32x2_t, z18,
	      svcvt_s32_f32_x2 (z23),
	      svcvt_s32 (z23))

/*
** cvt_z23_z28:
**	fcvtzs	[^\n]+, {z28\.s - z29\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z23_z28, svfloat32x2_t, svint32x2_t, z23,
	      svcvt_s32_f32_x2 (z28),
	      svcvt_s32 (z28))
