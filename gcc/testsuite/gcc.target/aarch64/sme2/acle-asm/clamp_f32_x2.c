/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** clamp_z24_z24_z0_z5:
**	fclamp	{z24\.s - z25\.s}, z0\.s, z5\.s
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z24_z0_z5, svfloat32x2_t, svfloat32_t, z24,
		svclamp_single_f32_x2 (z24, z0, z5),
		svclamp (z24, z0, z5))

/*
** clamp_z24_z28_z5_z7:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fclamp	{z24\.s - z25\.s}, z5\.s, z7\.s
** |
**	fclamp	{z28\.s - z29\.s}, z5\.s, z7\.s
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z28_z5_z7, svfloat32x2_t, svfloat32_t, z24,
		svclamp_single_f32_x2 (z28, z5, z7),
		svclamp (z28, z5, z7))

/*
** clamp_z24_z1_z7_z16:
** (
**	mov	z24\.d, z1\.d
**	mov	z25\.d, z2\.d
** |
**	mov	z25\.d, z2\.d
**	mov	z24\.d, z1\.d
** )
**	fclamp	{z24\.s - z25\.s}, z7\.s, z16\.s
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z1_z7_z16, svfloat32x2_t, svfloat32_t, z24,
		svclamp_single_f32_x2 (z1, z7, z16),
		svclamp (z1, z7, z16))

/*
** clamp_z1_z24_z16_z23:
**	fclamp	{z24\.s - z25\.s}, z16\.s, z23\.s
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z24_z16_z23, svfloat32x2_t, svfloat32_t, z1,
		svclamp_single_f32_x2 (z24, z16, z23),
		svclamp (z24, z16, z23))

/*
** clamp_z1_z1_z23_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	fclamp	{z[0-9]+\.s - z[0-9]+\.s}, z23\.s, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z1_z23_z0, svfloat32x2_t, svfloat32_t, z1,
		svclamp_single_f32_x2 (z1, z23, z0),
		svclamp (z1, z23, z0))

/*
** clamp_z18_z18_z0_z23:
**	fclamp	{z18\.s - z19\.s}, z0\.s, z23\.s
**	ret
*/
TEST_XN_SINGLE (clamp_z18_z18_z0_z23, svfloat32x2_t, svfloat32_t, z18,
		svclamp_single_f32_x2 (z18, z0, z23),
		svclamp (z18, z0, z23))

/*
** clamp_awkward:
**	...
**	fclamp	{z[0-9]+\.s - z[0-9]+\.s}, z[0-9]+\.s, z3\.s
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (clamp_awkward, svfloat32x2_t, svfloat32_t,
			z0_res = svclamp_single_f32_x2 (z1, z0, zn),
			z0_res = svclamp (z1, z0, zn))
