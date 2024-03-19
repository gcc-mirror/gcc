/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** minnm_z0_z0_z4:
**	fminnm	{z0\.d - z3\.d}, {z0\.d - z3\.d}, {z4\.d - z7\.d}
**	ret
*/
TEST_XN (minnm_z0_z0_z4, svfloat64x4_t, z0,
	 svminnm_f64_x4 (z0, z4),
	 svminnm (z0, z4))

/*
** minnm_z0_z4_z0:
**	fminnm	{z0\.d - z3\.d}, {z0\.d - z3\.d}, {z4\.d - z7\.d}
**	ret
*/
TEST_XN (minnm_z0_z4_z0, svfloat64x4_t, z0,
	 svminnm_f64_x4 (z4, z0),
	 svminnm (z4, z0))

/*
** minnm_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	[^\n]+, {z28\.d - z31\.d}
** |
**	fminnm	[^\n]+, {z28\.d - z31\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (minnm_z0_z4_z28, svfloat64x4_t, z0,
	 svminnm_f64_x4 (z4, z28),
	 svminnm (z4, z28))

/*
** minnm_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	[^\n]+, {z4\.d - z7\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (minnm_z18_z18_z4, svfloat64x4_t, z18,
	 svminnm_f64_x4 (z18, z4),
	 svminnm (z18, z4))

/*
** minnm_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	[^\n]+, {z28\.d - z31\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (minnm_z23_z23_z28, svfloat64x4_t, z23,
	 svminnm_f64_x4 (z23, z28),
	 svminnm (z23, z28))

/*
** minnm_z28_z28_z0:
**	fminnm	{z28\.d - z31\.d}, {z28\.d - z31\.d}, {z0\.d - z3\.d}
**	ret
*/
TEST_XN (minnm_z28_z28_z0, svfloat64x4_t, z28,
	 svminnm_f64_x4 (z28, z0),
	 svminnm (z28, z0))

/*
** minnm_z0_z0_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	{z0\.d - z3\.d}, {z0\.d - z3\.d}, [^\n]+
** |
**	fminnm	{z0\.d - z3\.d}, {z0\.d - z3\.d}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (minnm_z0_z0_z18, svfloat64x4_t, z0,
	 svminnm_f64_x4 (z0, z18),
	 svminnm (z0, z18))

/*
** minnm_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	{z4\.d - z7\.d}, {z4\.d - z7\.d}, [^\n]+
** |
**	fminnm	{z4\.d - z7\.d}, {z4\.d - z7\.d}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (minnm_z4_z4_z23, svfloat64x4_t, z4,
	 svminnm_f64_x4 (z4, z23),
	 svminnm (z4, z23))

/*
** minnm_single_z24_z24_z0:
**	fminnm	{z24\.d - z27\.d}, {z24\.d - z27\.d}, z0\.d
**	ret
*/
TEST_XN_SINGLE (minnm_single_z24_z24_z0, svfloat64x4_t, svfloat64_t, z24,
		svminnm_single_f64_x4 (z24, z0),
		svminnm (z24, z0))

/*
** minnm_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	{z24\.d - z27\.d}, {z24\.d - z27\.d}, z0\.d
** |
**	fminnm	{z28\.d - z31\.d}, {z28\.d - z31\.d}, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (minnm_single_z24_z28_z0, svfloat64x4_t, svfloat64_t, z24,
		svminnm_single_f64_x4 (z28, z0),
		svminnm (z28, z0))

/*
** minnm_single_z24_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	{z24\.d - z27\.d}, {z24\.d - z27\.d}, z0\.d
**	ret
*/
TEST_XN_SINGLE (minnm_single_z24_z1_z0, svfloat64x4_t, svfloat64_t, z24,
		svminnm_single_f64_x4 (z1, z0),
		svminnm (z1, z0))

/*
** minnm_single_z1_z24_z0:
**	fminnm	{z24\.d - z27\.d}, {z24\.d - z27\.d}, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (minnm_single_z1_z24_z0, svfloat64x4_t, svfloat64_t, z1,
		svminnm_single_f64_x4 (z24, z0),
		svminnm (z24, z0))

/*
** minnm_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	({z[0-9]+\.d - z[0-9]+\.d}), \1, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (minnm_single_z1_z1_z0, svfloat64x4_t, svfloat64_t, z1,
		svminnm_single_f64_x4 (z1, z0),
		svminnm (z1, z0))

/*
** minnm_single_z18_z18_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fminnm	[^\n]+, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (minnm_single_z18_z18_z0, svfloat64x4_t, svfloat64_t, z18,
		svminnm_single_f64_x4 (z18, z0),
		svminnm (z18, z0))

/*
** minnm_single_awkward:
**	...
**	fminnm	({z[0-9]+\.d - z[0-9]+\.d}), \1, z[0-9]+\.d
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (minnm_single_awkward, svfloat64x4_t, svfloat64_t,
			z0_res = svminnm_single_f64_x4 (z1, z0),
			z0_res = svminnm (z1, z0))

/*
** minnm_single_z0_z0_z15:
**	...
**	fminnm	{z0\.d - z3\.d}, {z0\.d - z3\.d}, z15\.d
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (minnm_single_z0_z0_z15, svfloat64x4_t, svfloat64_t,
		    z0 = svminnm_single_f64_x4 (z0, z15),
		    z0 = svminnm (z0, z15))

/*
** minnm_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	fminnm	{z24\.d - z27\.d}, {z24\.d - z27\.d}, \1\.d
**	ret
*/
TEST_XN_SINGLE (minnm_single_z24_z24_z16, svfloat64x4_t, svfloat64_t, z24,
		svminnm_single_f64_x4 (z24, z16),
		svminnm (z24, z16))
