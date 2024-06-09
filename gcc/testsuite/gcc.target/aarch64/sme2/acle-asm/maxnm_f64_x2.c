/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** maxnm_z0_z0_z4:
**	fmaxnm	{z0\.d - z1\.d}, {z0\.d - z1\.d}, {z4\.d - z5\.d}
**	ret
*/
TEST_XN (maxnm_z0_z0_z4, svfloat64x2_t, z0,
	 svmaxnm_f64_x2 (z0, z4),
	 svmaxnm (z0, z4))

/*
** maxnm_z0_z4_z0:
**	fmaxnm	{z0\.d - z1\.d}, {z0\.d - z1\.d}, {z4\.d - z5\.d}
**	ret
*/
TEST_XN (maxnm_z0_z4_z0, svfloat64x2_t, z0,
	 svmaxnm_f64_x2 (z4, z0),
	 svmaxnm (z4, z0))

/*
** maxnm_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fmaxnm	[^\n]+, {z28\.d - z29\.d}
** |
**	fmaxnm	[^\n]+, {z28\.d - z29\.d}
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (maxnm_z0_z4_z28, svfloat64x2_t, z0,
	 svmaxnm_f64_x2 (z4, z28),
	 svmaxnm (z4, z28))

/*
** maxnm_z18_z18_z4:
**	fmaxnm	{z18\.d - z19\.d}, {z18\.d - z19\.d}, {z4\.d - z5\.d}
**	ret
*/
TEST_XN (maxnm_z18_z18_z4, svfloat64x2_t, z18,
	 svmaxnm_f64_x2 (z18, z4),
	 svmaxnm (z18, z4))

/*
** maxnm_z23_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	fmaxnm	[^\n]+, {z18\.d - z19\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (maxnm_z23_z23_z18, svfloat64x2_t, z23,
	 svmaxnm_f64_x2 (z23, z18),
	 svmaxnm (z23, z18))

/*
** maxnm_z28_z28_z0:
**	fmaxnm	{z28\.d - z29\.d}, {z28\.d - z29\.d}, {z0\.d - z1\.d}
**	ret
*/
TEST_XN (maxnm_z28_z28_z0, svfloat64x2_t, z28,
	 svmaxnm_f64_x2 (z28, z0),
	 svmaxnm (z28, z0))

/*
** maxnm_z0_z0_z18:
**	fmaxnm	{z0\.d - z1\.d}, {z0\.d - z1\.d}, {z18\.d - z19\.d}
**	ret
*/
TEST_XN (maxnm_z0_z0_z18, svfloat64x2_t, z0,
	 svmaxnm_f64_x2 (z0, z18),
	 svmaxnm (z0, z18))

/*
** maxnm_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fmaxnm	{z4\.d - z5\.d}, {z4\.d - z5\.d}, [^\n]+
** |
**	fmaxnm	{z4\.d - z5\.d}, {z4\.d - z5\.d}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (maxnm_z4_z4_z23, svfloat64x2_t, z4,
	 svmaxnm_f64_x2 (z4, z23),
	 svmaxnm (z4, z23))

/*
** maxnm_single_z24_z24_z0:
**	fmaxnm	{z24\.d - z25\.d}, {z24\.d - z25\.d}, z0\.d
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z24_z24_z0, svfloat64x2_t, svfloat64_t, z24,
		svmaxnm_single_f64_x2 (z24, z0),
		svmaxnm (z24, z0))

/*
** maxnm_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fmaxnm	{z24\.d - z25\.d}, {z24\.d - z25\.d}, z0\.d
** |
**	fmaxnm	{z28\.d - z29\.d}, {z28\.d - z29\.d}, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z24_z28_z0, svfloat64x2_t, svfloat64_t, z24,
		svmaxnm_single_f64_x2 (z28, z0),
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
**	fmaxnm	{z24\.d - z25\.d}, {z24\.d - z25\.d}, z0\.d
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z24_z1_z0, svfloat64x2_t, svfloat64_t, z24,
		svmaxnm_single_f64_x2 (z1, z0),
		svmaxnm (z1, z0))

/*
** maxnm_single_z1_z24_z0:
**	fmaxnm	{z24\.d - z25\.d}, {z24\.d - z25\.d}, z0\.d
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z1_z24_z0, svfloat64x2_t, svfloat64_t, z1,
		svmaxnm_single_f64_x2 (z24, z0),
		svmaxnm (z24, z0))

/*
** maxnm_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	fmaxnm	({z[0-9]+\.d - z[0-9]+\.d}), \1, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z1_z1_z0, svfloat64x2_t, svfloat64_t, z1,
		svmaxnm_single_f64_x2 (z1, z0),
		svmaxnm (z1, z0))

/*
** maxnm_single_z18_z18_z0:
**	fmaxnm	{z18\.d - z19\.d}, {z18\.d - z19\.d}, z0\.d
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z18_z18_z0, svfloat64x2_t, svfloat64_t, z18,
		svmaxnm_single_f64_x2 (z18, z0),
		svmaxnm (z18, z0))

/*
** maxnm_single_awkward:
**	...
**	fmaxnm	({z[0-9]+\.d - z[0-9]+\.d}), \1, z[0-9]+\.d
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (maxnm_single_awkward, svfloat64x2_t, svfloat64_t,
			z0_res = svmaxnm_single_f64_x2 (z1, z0),
			z0_res = svmaxnm (z1, z0))

/*
** maxnm_single_z0_z0_z15:
**	...
**	fmaxnm	{z0\.d - z1\.d}, {z0\.d - z1\.d}, z15\.d
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (maxnm_single_z0_z0_z15, svfloat64x2_t, svfloat64_t,
		    z0 = svmaxnm_single_f64_x2 (z0, z15),
		    z0 = svmaxnm (z0, z15))

/*
** maxnm_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	fmaxnm	{z24\.d - z25\.d}, {z24\.d - z25\.d}, \1\.d
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z24_z24_z16, svfloat64x2_t, svfloat64_t, z24,
		svmaxnm_single_f64_x2 (z24, z16),
		svmaxnm (z24, z16))
