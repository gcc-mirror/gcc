/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** cvt_z0_z4:
**	fcvtzu	{z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_DUAL_XN (cvt_z0_z4, svuint32x4_t, svfloat32x4_t, z0,
	      svcvt_u32_f32_x4 (z4),
	      svcvt_u32 (z4))

/*
** cvt_z4_z0:
**	fcvtzu	{z4\.s - z7\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_DUAL_XN (cvt_z4_z0, svfloat32x4_t, svuint32x4_t, z4,
	      svcvt_u32_f32_x4 (z0),
	      svcvt_u32 (z0))

/*
** cvt_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvtzu	{z4\.s - z7\.s}, [^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z4_z18, svfloat32x4_t, svuint32x4_t, z4,
	      svcvt_u32_f32_x4 (z18),
	      svcvt_u32 (z18))

/*
** cvt_z28_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvtzu	{z28\.s - z31\.s}, [^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z28_z23, svuint32x4_t, svfloat32x4_t, z28,
	      svcvt_u32_f32_x4 (z23),
	      svcvt_u32 (z23))

/*
** cvt_z23_z28:
**	fcvtzu	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z23_z28, svfloat32x4_t, svuint32x4_t, z23,
	      svcvt_u32_f32_x4 (z28),
	      svcvt_u32 (z28))

/*
** cvt_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvtzu	{z[^\n]+}, {z.*}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z23_z18, svfloat32x4_t, svuint32x4_t, z23,
	      svcvt_u32_f32_x4 (z18),
	      svcvt_u32 (z18))
