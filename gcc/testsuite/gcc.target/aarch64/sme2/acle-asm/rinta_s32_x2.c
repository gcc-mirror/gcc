/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** rinta_z0_z0:
**	frinta	{z0\.s - z1\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (rinta_z0_z0, svfloat32x2_t, z0,
	 svrinta_f32_x2 (z0),
	 svrinta (z0))

/*
** rinta_z0_z4:
**	frinta	{z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (rinta_z0_z4, svfloat32x2_t, z0,
	 svrinta_f32_x2 (z4),
	 svrinta (z4))

/*
** rinta_z4_z18:
**	frinta	{z4\.s - z5\.s}, {z18\.s - z19\.s}
**	ret
*/
TEST_XN (rinta_z4_z18, svfloat32x2_t, z4,
	 svrinta_f32_x2 (z18),
	 svrinta (z18))

/*
** rinta_z18_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	frinta	{z18\.s - z19\.s}, [^\n]+
**	ret
*/
TEST_XN (rinta_z18_z23, svfloat32x2_t, z18,
	 svrinta_f32_x2 (z23),
	 svrinta (z23))

/*
** rinta_z23_z28:
**	frinta	[^\n]+, {z28\.s - z29\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rinta_z23_z28, svfloat32x2_t, z23,
	 svrinta_f32_x2 (z28),
	 svrinta (z28))

/*
** rinta_z28_z0:
**	frinta	{z28\.s - z29\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (rinta_z28_z0, svfloat32x2_t, z28,
	 svrinta_f32_x2 (z0),
	 svrinta (z0))
