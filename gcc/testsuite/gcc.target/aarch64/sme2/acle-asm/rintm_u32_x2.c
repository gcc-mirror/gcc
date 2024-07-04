/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** rintm_z0_z0:
**	frintm	{z0\.s - z1\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (rintm_z0_z0, svfloat32x2_t, z0,
	 svrintm_f32_x2 (z0),
	 svrintm (z0))

/*
** rintm_z0_z4:
**	frintm	{z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (rintm_z0_z4, svfloat32x2_t, z0,
	 svrintm_f32_x2 (z4),
	 svrintm (z4))

/*
** rintm_z4_z18:
**	frintm	{z4\.s - z5\.s}, {z18\.s - z19\.s}
**	ret
*/
TEST_XN (rintm_z4_z18, svfloat32x2_t, z4,
	 svrintm_f32_x2 (z18),
	 svrintm (z18))

/*
** rintm_z18_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	frintm	{z18\.s - z19\.s}, [^\n]+
**	ret
*/
TEST_XN (rintm_z18_z23, svfloat32x2_t, z18,
	 svrintm_f32_x2 (z23),
	 svrintm (z23))

/*
** rintm_z23_z28:
**	frintm	[^\n]+, {z28\.s - z29\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rintm_z23_z28, svfloat32x2_t, z23,
	 svrintm_f32_x2 (z28),
	 svrintm (z28))

/*
** rintm_z28_z0:
**	frintm	{z28\.s - z29\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (rintm_z28_z0, svfloat32x2_t, z28,
	 svrintm_f32_x2 (z0),
	 svrintm (z0))
