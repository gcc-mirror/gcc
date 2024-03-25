/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** cvt_z0_z4:
**	fcvtzs	{z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_DUAL_XN (cvt_z0_z4, svint32x4_t, svfloat32x4_t, z0,
	      svcvt_s32_f32_x4 (z4),
	      svcvt_s32 (z4))

/*
** cvt_z4_z0:
**	fcvtzs	{z4\.s - z7\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_DUAL_XN (cvt_z4_z0, svfloat32x4_t, svint32x4_t, z4,
	      svcvt_s32_f32_x4 (z0),
	      svcvt_s32 (z0))

/*
** cvt_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvtzs	{z4\.s - z7\.s}, [^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z4_z18, svfloat32x4_t, svint32x4_t, z4,
	      svcvt_s32_f32_x4 (z18),
	      svcvt_s32 (z18))

/*
** cvt_z28_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvtzs	{z28\.s - z31\.s}, [^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z28_z23, svint32x4_t, svfloat32x4_t, z28,
	      svcvt_s32_f32_x4 (z23),
	      svcvt_s32 (z23))

/*
** cvt_z23_z28:
**	fcvtzs	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z23_z28, svfloat32x4_t, svint32x4_t, z23,
	      svcvt_s32_f32_x4 (z28),
	      svcvt_s32 (z28))

/*
** cvt_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvtzs	{z[^\n]+}, {z.*}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z23_z18, svfloat32x4_t, svint32x4_t, z23,
	      svcvt_s32_f32_x4 (z18),
	      svcvt_s32 (z18))
