/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** rinta_z0_z0:
**	frinta	{z0\.s - z3\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (rinta_z0_z0, svfloat32x4_t, z0,
	 svrinta_f32_x4 (z0),
	 svrinta (z0))

/*
** rinta_z0_z4:
**	frinta	{z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_XN (rinta_z0_z4, svfloat32x4_t, z0,
	 svrinta_f32_x4 (z4),
	 svrinta (z4))

/*
** rinta_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	frinta	{z4\.s - z7\.s}, [^\n]+
**	ret
*/
TEST_XN (rinta_z4_z18, svfloat32x4_t, z4,
	 svrinta_f32_x4 (z18),
	 svrinta (z18))

/*
** rinta_z18_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	frinta	{z[^\n]+}, {z.*}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rinta_z18_z23, svfloat32x4_t, z18,
	 svrinta_f32_x4 (z23),
	 svrinta (z23))

/*
** rinta_z23_z28:
**	frinta	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rinta_z23_z28, svfloat32x4_t, z23,
	 svrinta_f32_x4 (z28),
	 svrinta (z28))

/*
** rinta_z28_z0:
**	frinta	{z28\.s - z31\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (rinta_z28_z0, svfloat32x4_t, z28,
	 svrinta_f32_x4 (z0),
	 svrinta (z0))
