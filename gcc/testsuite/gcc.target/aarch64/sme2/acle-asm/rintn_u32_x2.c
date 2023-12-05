/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** rintn_z0_z0:
**	frintn	{z0\.s - z1\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (rintn_z0_z0, svfloat32x2_t, z0,
	 svrintn_f32_x2 (z0),
	 svrintn (z0))

/*
** rintn_z0_z4:
**	frintn	{z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (rintn_z0_z4, svfloat32x2_t, z0,
	 svrintn_f32_x2 (z4),
	 svrintn (z4))

/*
** rintn_z4_z18:
**	frintn	{z4\.s - z5\.s}, {z18\.s - z19\.s}
**	ret
*/
TEST_XN (rintn_z4_z18, svfloat32x2_t, z4,
	 svrintn_f32_x2 (z18),
	 svrintn (z18))

/*
** rintn_z18_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	frintn	{z18\.s - z19\.s}, [^\n]+
**	ret
*/
TEST_XN (rintn_z18_z23, svfloat32x2_t, z18,
	 svrintn_f32_x2 (z23),
	 svrintn (z23))

/*
** rintn_z23_z28:
**	frintn	[^\n]+, {z28\.s - z29\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rintn_z23_z28, svfloat32x2_t, z23,
	 svrintn_f32_x2 (z28),
	 svrintn (z28))

/*
** rintn_z28_z0:
**	frintn	{z28\.s - z29\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (rintn_z28_z0, svfloat32x2_t, z28,
	 svrintn_f32_x2 (z0),
	 svrintn (z0))
