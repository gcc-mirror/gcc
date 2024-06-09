/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** cvt_z0_z4:
**	ucvtf	{z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_DUAL_XN (cvt_z0_z4, svfloat32x4_t, svuint32x4_t, z0,
	      svcvt_f32_u32_x4 (z4),
	      svcvt_f32 (z4))

/*
** cvt_z4_z0:
**	ucvtf	{z4\.s - z7\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_DUAL_XN (cvt_z4_z0, svuint32x4_t, svfloat32x4_t, z4,
	      svcvt_f32_u32_x4 (z0),
	      svcvt_f32 (z0))

/*
** cvt_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ucvtf	{z4\.s - z7\.s}, [^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z4_z18, svuint32x4_t, svfloat32x4_t, z4,
	      svcvt_f32_u32_x4 (z18),
	      svcvt_f32 (z18))

/*
** cvt_z28_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ucvtf	{z28\.s - z31\.s}, [^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z28_z23, svfloat32x4_t, svuint32x4_t, z28,
	      svcvt_f32_u32_x4 (z23),
	      svcvt_f32 (z23))

/*
** cvt_z23_z28:
**	ucvtf	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z23_z28, svuint32x4_t, svfloat32x4_t, z23,
	      svcvt_f32_u32_x4 (z28),
	      svcvt_f32 (z28))

/*
** cvt_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ucvtf	{z[^\n]+}, {z.*}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z23_z18, svuint32x4_t, svfloat32x4_t, z23,
	      svcvt_f32_u32_x4 (z18),
	      svcvt_f32 (z18))
