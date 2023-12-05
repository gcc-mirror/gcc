/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** rintp_z0_z0:
**	frintp	{z0\.s - z1\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (rintp_z0_z0, svfloat32x2_t, z0,
	 svrintp_f32_x2 (z0),
	 svrintp (z0))

/*
** rintp_z0_z4:
**	frintp	{z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (rintp_z0_z4, svfloat32x2_t, z0,
	 svrintp_f32_x2 (z4),
	 svrintp (z4))

/*
** rintp_z4_z18:
**	frintp	{z4\.s - z5\.s}, {z18\.s - z19\.s}
**	ret
*/
TEST_XN (rintp_z4_z18, svfloat32x2_t, z4,
	 svrintp_f32_x2 (z18),
	 svrintp (z18))

/*
** rintp_z18_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	frintp	{z18\.s - z19\.s}, [^\n]+
**	ret
*/
TEST_XN (rintp_z18_z23, svfloat32x2_t, z18,
	 svrintp_f32_x2 (z23),
	 svrintp (z23))

/*
** rintp_z23_z28:
**	frintp	[^\n]+, {z28\.s - z29\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rintp_z23_z28, svfloat32x2_t, z23,
	 svrintp_f32_x2 (z28),
	 svrintp (z28))

/*
** rintp_z28_z0:
**	frintp	{z28\.s - z29\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (rintp_z28_z0, svfloat32x2_t, z28,
	 svrintp_f32_x2 (z0),
	 svrintp (z0))
