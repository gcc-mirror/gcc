/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** maxnm_z0_z0_z4:
**	fmaxnm	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (maxnm_z0_z0_z4, svfloat32x2_t, z0,
	 svmaxnm_f32_x2 (z0, z4),
	 svmaxnm (z0, z4))

/*
** maxnm_z0_z4_z0:
**	fmaxnm	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (maxnm_z0_z4_z0, svfloat32x2_t, z0,
	 svmaxnm_f32_x2 (z4, z0),
	 svmaxnm (z4, z0))

/*
** maxnm_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fmaxnm	[^\n]+, {z28\.s - z29\.s}
** |
**	fmaxnm	[^\n]+, {z28\.s - z29\.s}
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (maxnm_z0_z4_z28, svfloat32x2_t, z0,
	 svmaxnm_f32_x2 (z4, z28),
	 svmaxnm (z4, z28))

/*
** maxnm_z18_z18_z4:
**	fmaxnm	{z18\.s - z19\.s}, {z18\.s - z19\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (maxnm_z18_z18_z4, svfloat32x2_t, z18,
	 svmaxnm_f32_x2 (z18, z4),
	 svmaxnm (z18, z4))

/*
** maxnm_z23_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	fmaxnm	[^\n]+, {z18\.s - z19\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (maxnm_z23_z23_z18, svfloat32x2_t, z23,
	 svmaxnm_f32_x2 (z23, z18),
	 svmaxnm (z23, z18))

/*
** maxnm_z28_z28_z0:
**	fmaxnm	{z28\.s - z29\.s}, {z28\.s - z29\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (maxnm_z28_z28_z0, svfloat32x2_t, z28,
	 svmaxnm_f32_x2 (z28, z0),
	 svmaxnm (z28, z0))

/*
** maxnm_z0_z0_z18:
**	fmaxnm	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z18\.s - z19\.s}
**	ret
*/
TEST_XN (maxnm_z0_z0_z18, svfloat32x2_t, z0,
	 svmaxnm_f32_x2 (z0, z18),
	 svmaxnm (z0, z18))

/*
** maxnm_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fmaxnm	{z4\.s - z5\.s}, {z4\.s - z5\.s}, [^\n]+
** |
**	fmaxnm	{z4\.s - z5\.s}, {z4\.s - z5\.s}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (maxnm_z4_z4_z23, svfloat32x2_t, z4,
	 svmaxnm_f32_x2 (z4, z23),
	 svmaxnm (z4, z23))

/*
** maxnm_single_z24_z24_z0:
**	fmaxnm	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z24_z24_z0, svfloat32x2_t, svfloat32_t, z24,
		svmaxnm_single_f32_x2 (z24, z0),
		svmaxnm (z24, z0))

/*
** maxnm_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fmaxnm	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
** |
**	fmaxnm	{z28\.s - z29\.s}, {z28\.s - z29\.s}, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z24_z28_z0, svfloat32x2_t, svfloat32_t, z24,
		svmaxnm_single_f32_x2 (z28, z0),
		svmaxnm (z28, z0))

/*
** maxnm_single_z24_z1_z0:
** (
**	mov	z24\.d, z1\.d
**	mov	z25\.d, z2\.d
** |
**	mov	z25\.d, z2\.d
**	mov	z24\.d, z1\.d
** )
**	fmaxnm	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z24_z1_z0, svfloat32x2_t, svfloat32_t, z24,
		svmaxnm_single_f32_x2 (z1, z0),
		svmaxnm (z1, z0))

/*
** maxnm_single_z1_z24_z0:
**	fmaxnm	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z1_z24_z0, svfloat32x2_t, svfloat32_t, z1,
		svmaxnm_single_f32_x2 (z24, z0),
		svmaxnm (z24, z0))

/*
** maxnm_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	fmaxnm	({z[0-9]+\.s - z[0-9]+\.s}), \1, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z1_z1_z0, svfloat32x2_t, svfloat32_t, z1,
		svmaxnm_single_f32_x2 (z1, z0),
		svmaxnm (z1, z0))

/*
** maxnm_single_z18_z18_z0:
**	fmaxnm	{z18\.s - z19\.s}, {z18\.s - z19\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z18_z18_z0, svfloat32x2_t, svfloat32_t, z18,
		svmaxnm_single_f32_x2 (z18, z0),
		svmaxnm (z18, z0))

/*
** maxnm_single_awkward:
**	...
**	fmaxnm	({z[0-9]+\.s - z[0-9]+\.s}), \1, z[0-9]+\.s
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (maxnm_single_awkward, svfloat32x2_t, svfloat32_t,
			z0_res = svmaxnm_single_f32_x2 (z1, z0),
			z0_res = svmaxnm (z1, z0))

/*
** maxnm_single_z0_z0_z15:
**	...
**	fmaxnm	{z0\.s - z1\.s}, {z0\.s - z1\.s}, z15\.s
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (maxnm_single_z0_z0_z15, svfloat32x2_t, svfloat32_t,
		    z0 = svmaxnm_single_f32_x2 (z0, z15),
		    z0 = svmaxnm (z0, z15))

/*
** maxnm_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	fmaxnm	{z24\.s - z25\.s}, {z24\.s - z25\.s}, \1\.s
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z24_z24_z16, svfloat32x2_t, svfloat32_t, z24,
		svmaxnm_single_f32_x2 (z24, z16),
		svmaxnm (z24, z16))
