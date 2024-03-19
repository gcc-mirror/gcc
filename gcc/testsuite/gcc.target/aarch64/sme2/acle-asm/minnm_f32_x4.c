/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** minnm_z0_z0_z4:
**	fminnm	{z0\.s - z3\.s}, {z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_XN (minnm_z0_z0_z4, svfloat32x4_t, z0,
	 svminnm_f32_x4 (z0, z4),
	 svminnm (z0, z4))

/*
** minnm_z0_z4_z0:
**	fminnm	{z0\.s - z3\.s}, {z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_XN (minnm_z0_z4_z0, svfloat32x4_t, z0,
	 svminnm_f32_x4 (z4, z0),
	 svminnm (z4, z0))

/*
** minnm_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	[^\n]+, {z28\.s - z31\.s}
** |
**	fminnm	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (minnm_z0_z4_z28, svfloat32x4_t, z0,
	 svminnm_f32_x4 (z4, z28),
	 svminnm (z4, z28))

/*
** minnm_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	[^\n]+, {z4\.s - z7\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (minnm_z18_z18_z4, svfloat32x4_t, z18,
	 svminnm_f32_x4 (z18, z4),
	 svminnm (z18, z4))

/*
** minnm_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (minnm_z23_z23_z28, svfloat32x4_t, z23,
	 svminnm_f32_x4 (z23, z28),
	 svminnm (z23, z28))

/*
** minnm_z28_z28_z0:
**	fminnm	{z28\.s - z31\.s}, {z28\.s - z31\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (minnm_z28_z28_z0, svfloat32x4_t, z28,
	 svminnm_f32_x4 (z28, z0),
	 svminnm (z28, z0))

/*
** minnm_z0_z0_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	{z0\.s - z3\.s}, {z0\.s - z3\.s}, [^\n]+
** |
**	fminnm	{z0\.s - z3\.s}, {z0\.s - z3\.s}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (minnm_z0_z0_z18, svfloat32x4_t, z0,
	 svminnm_f32_x4 (z0, z18),
	 svminnm (z0, z18))

/*
** minnm_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	{z4\.s - z7\.s}, {z4\.s - z7\.s}, [^\n]+
** |
**	fminnm	{z4\.s - z7\.s}, {z4\.s - z7\.s}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (minnm_z4_z4_z23, svfloat32x4_t, z4,
	 svminnm_f32_x4 (z4, z23),
	 svminnm (z4, z23))

/*
** minnm_single_z24_z24_z0:
**	fminnm	{z24\.s - z27\.s}, {z24\.s - z27\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (minnm_single_z24_z24_z0, svfloat32x4_t, svfloat32_t, z24,
		svminnm_single_f32_x4 (z24, z0),
		svminnm (z24, z0))

/*
** minnm_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	{z24\.s - z27\.s}, {z24\.s - z27\.s}, z0\.s
** |
**	fminnm	{z28\.s - z31\.s}, {z28\.s - z31\.s}, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (minnm_single_z24_z28_z0, svfloat32x4_t, svfloat32_t, z24,
		svminnm_single_f32_x4 (z28, z0),
		svminnm (z28, z0))

/*
** minnm_single_z24_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	{z24\.s - z27\.s}, {z24\.s - z27\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (minnm_single_z24_z1_z0, svfloat32x4_t, svfloat32_t, z24,
		svminnm_single_f32_x4 (z1, z0),
		svminnm (z1, z0))

/*
** minnm_single_z1_z24_z0:
**	fminnm	{z24\.s - z27\.s}, {z24\.s - z27\.s}, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (minnm_single_z1_z24_z0, svfloat32x4_t, svfloat32_t, z1,
		svminnm_single_f32_x4 (z24, z0),
		svminnm (z24, z0))

/*
** minnm_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	({z[0-9]+\.s - z[0-9]+\.s}), \1, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (minnm_single_z1_z1_z0, svfloat32x4_t, svfloat32_t, z1,
		svminnm_single_f32_x4 (z1, z0),
		svminnm (z1, z0))

/*
** minnm_single_z18_z18_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	[^\n]+, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (minnm_single_z18_z18_z0, svfloat32x4_t, svfloat32_t, z18,
		svminnm_single_f32_x4 (z18, z0),
		svminnm (z18, z0))

/*
** minnm_single_awkward:
**	...
**	fminnm	({z[0-9]+\.s - z[0-9]+\.s}), \1, z[0-9]+\.s
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (minnm_single_awkward, svfloat32x4_t, svfloat32_t,
			z0_res = svminnm_single_f32_x4 (z1, z0),
			z0_res = svminnm (z1, z0))

/*
** minnm_single_z0_z0_z15:
**	...
**	fminnm	{z0\.s - z3\.s}, {z0\.s - z3\.s}, z15\.s
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (minnm_single_z0_z0_z15, svfloat32x4_t, svfloat32_t,
		    z0 = svminnm_single_f32_x4 (z0, z15),
		    z0 = svminnm (z0, z15))

/*
** minnm_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	fminnm	{z24\.s - z27\.s}, {z24\.s - z27\.s}, \1\.s
**	ret
*/
TEST_XN_SINGLE (minnm_single_z24_z24_z16, svfloat32x4_t, svfloat32_t, z24,
		svminnm_single_f32_x4 (z24, z16),
		svminnm (z24, z16))
