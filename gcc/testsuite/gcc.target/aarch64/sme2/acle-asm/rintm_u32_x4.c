/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** rintm_z0_z0:
**	frintm	{z0\.s - z3\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (rintm_z0_z0, svfloat32x4_t, z0,
	 svrintm_f32_x4 (z0),
	 svrintm (z0))

/*
** rintm_z0_z4:
**	frintm	{z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_XN (rintm_z0_z4, svfloat32x4_t, z0,
	 svrintm_f32_x4 (z4),
	 svrintm (z4))

/*
** rintm_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	frintm	{z4\.s - z7\.s}, [^\n]+
**	ret
*/
TEST_XN (rintm_z4_z18, svfloat32x4_t, z4,
	 svrintm_f32_x4 (z18),
	 svrintm (z18))

/*
** rintm_z18_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	frintm	{z[^\n]+}, {z.*}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rintm_z18_z23, svfloat32x4_t, z18,
	 svrintm_f32_x4 (z23),
	 svrintm (z23))

/*
** rintm_z23_z28:
**	frintm	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rintm_z23_z28, svfloat32x4_t, z23,
	 svrintm_f32_x4 (z28),
	 svrintm (z28))

/*
** rintm_z28_z0:
**	frintm	{z28\.s - z31\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (rintm_z28_z0, svfloat32x4_t, z28,
	 svrintm_f32_x4 (z0),
	 svrintm (z0))
