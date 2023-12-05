/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** rintp_z0_z0:
**	frintp	{z0\.s - z3\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (rintp_z0_z0, svfloat32x4_t, z0,
	 svrintp_f32_x4 (z0),
	 svrintp (z0))

/*
** rintp_z0_z4:
**	frintp	{z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_XN (rintp_z0_z4, svfloat32x4_t, z0,
	 svrintp_f32_x4 (z4),
	 svrintp (z4))

/*
** rintp_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	frintp	{z4\.s - z7\.s}, [^\n]+
**	ret
*/
TEST_XN (rintp_z4_z18, svfloat32x4_t, z4,
	 svrintp_f32_x4 (z18),
	 svrintp (z18))

/*
** rintp_z18_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	frintp	{z[^\n]+}, {z.*}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rintp_z18_z23, svfloat32x4_t, z18,
	 svrintp_f32_x4 (z23),
	 svrintp (z23))

/*
** rintp_z23_z28:
**	frintp	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rintp_z23_z28, svfloat32x4_t, z23,
	 svrintp_f32_x4 (z28),
	 svrintp (z28))

/*
** rintp_z28_z0:
**	frintp	{z28\.s - z31\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (rintp_z28_z0, svfloat32x4_t, z28,
	 svrintp_f32_x4 (z0),
	 svrintp (z0))
