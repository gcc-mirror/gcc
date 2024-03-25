/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** rintn_z0_z0:
**	frintn	{z0\.s - z3\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (rintn_z0_z0, svfloat32x4_t, z0,
	 svrintn_f32_x4 (z0),
	 svrintn (z0))

/*
** rintn_z0_z4:
**	frintn	{z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_XN (rintn_z0_z4, svfloat32x4_t, z0,
	 svrintn_f32_x4 (z4),
	 svrintn (z4))

/*
** rintn_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	frintn	{z4\.s - z7\.s}, [^\n]+
**	ret
*/
TEST_XN (rintn_z4_z18, svfloat32x4_t, z4,
	 svrintn_f32_x4 (z18),
	 svrintn (z18))

/*
** rintn_z18_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	frintn	{z[^\n]+}, {z.*}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rintn_z18_z23, svfloat32x4_t, z18,
	 svrintn_f32_x4 (z23),
	 svrintn (z23))

/*
** rintn_z23_z28:
**	frintn	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rintn_z23_z28, svfloat32x4_t, z23,
	 svrintn_f32_x4 (z28),
	 svrintn (z28))

/*
** rintn_z28_z0:
**	frintn	{z28\.s - z31\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (rintn_z28_z0, svfloat32x4_t, z28,
	 svrintn_f32_x4 (z0),
	 svrintn (z0))
