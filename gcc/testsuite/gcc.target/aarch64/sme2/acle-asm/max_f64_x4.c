/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** max_z0_z0_z4:
**	fmax	{z0\.d - z3\.d}, {z0\.d - z3\.d}, {z4\.d - z7\.d}
**	ret
*/
TEST_XN (max_z0_z0_z4, svfloat64x4_t, z0,
	 svmax_f64_x4 (z0, z4),
	 svmax (z0, z4))

/*
** max_z0_z4_z0:
**	fmax	{z0\.d - z3\.d}, {z0\.d - z3\.d}, {z4\.d - z7\.d}
**	ret
*/
TEST_XN (max_z0_z4_z0, svfloat64x4_t, z0,
	 svmax_f64_x4 (z4, z0),
	 svmax (z4, z0))

/*
** max_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	[^\n]+, {z28\.d - z31\.d}
** |
**	fmax	[^\n]+, {z28\.d - z31\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (max_z0_z4_z28, svfloat64x4_t, z0,
	 svmax_f64_x4 (z4, z28),
	 svmax (z4, z28))

/*
** max_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	[^\n]+, {z4\.d - z7\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (max_z18_z18_z4, svfloat64x4_t, z18,
	 svmax_f64_x4 (z18, z4),
	 svmax (z18, z4))

/*
** max_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	[^\n]+, {z28\.d - z31\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (max_z23_z23_z28, svfloat64x4_t, z23,
	 svmax_f64_x4 (z23, z28),
	 svmax (z23, z28))

/*
** max_z28_z28_z0:
**	fmax	{z28\.d - z31\.d}, {z28\.d - z31\.d}, {z0\.d - z3\.d}
**	ret
*/
TEST_XN (max_z28_z28_z0, svfloat64x4_t, z28,
	 svmax_f64_x4 (z28, z0),
	 svmax (z28, z0))

/*
** max_z0_z0_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	{z0\.d - z3\.d}, {z0\.d - z3\.d}, [^\n]+
** |
**	fmax	{z0\.d - z3\.d}, {z0\.d - z3\.d}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (max_z0_z0_z18, svfloat64x4_t, z0,
	 svmax_f64_x4 (z0, z18),
	 svmax (z0, z18))

/*
** max_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	{z4\.d - z7\.d}, {z4\.d - z7\.d}, [^\n]+
** |
**	fmax	{z4\.d - z7\.d}, {z4\.d - z7\.d}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (max_z4_z4_z23, svfloat64x4_t, z4,
	 svmax_f64_x4 (z4, z23),
	 svmax (z4, z23))

/*
** max_single_z24_z24_z0:
**	fmax	{z24\.d - z27\.d}, {z24\.d - z27\.d}, z0\.d
**	ret
*/
TEST_XN_SINGLE (max_single_z24_z24_z0, svfloat64x4_t, svfloat64_t, z24,
		svmax_single_f64_x4 (z24, z0),
		svmax (z24, z0))

/*
** max_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	{z24\.d - z27\.d}, {z24\.d - z27\.d}, z0\.d
** |
**	fmax	{z28\.d - z31\.d}, {z28\.d - z31\.d}, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (max_single_z24_z28_z0, svfloat64x4_t, svfloat64_t, z24,
		svmax_single_f64_x4 (z28, z0),
		svmax (z28, z0))

/*
** max_single_z24_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	{z24\.d - z27\.d}, {z24\.d - z27\.d}, z0\.d
**	ret
*/
TEST_XN_SINGLE (max_single_z24_z1_z0, svfloat64x4_t, svfloat64_t, z24,
		svmax_single_f64_x4 (z1, z0),
		svmax (z1, z0))

/*
** max_single_z1_z24_z0:
**	fmax	{z24\.d - z27\.d}, {z24\.d - z27\.d}, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (max_single_z1_z24_z0, svfloat64x4_t, svfloat64_t, z1,
		svmax_single_f64_x4 (z24, z0),
		svmax (z24, z0))

/*
** max_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	({z[0-9]+\.d - z[0-9]+\.d}), \1, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (max_single_z1_z1_z0, svfloat64x4_t, svfloat64_t, z1,
		svmax_single_f64_x4 (z1, z0),
		svmax (z1, z0))

/*
** max_single_z18_z18_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	[^\n]+, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (max_single_z18_z18_z0, svfloat64x4_t, svfloat64_t, z18,
		svmax_single_f64_x4 (z18, z0),
		svmax (z18, z0))

/*
** max_single_awkward:
**	...
**	fmax	({z[0-9]+\.d - z[0-9]+\.d}), \1, z[0-9]+\.d
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (max_single_awkward, svfloat64x4_t, svfloat64_t,
			z0_res = svmax_single_f64_x4 (z1, z0),
			z0_res = svmax (z1, z0))

/*
** max_single_z0_z0_z15:
**	...
**	fmax	{z0\.d - z3\.d}, {z0\.d - z3\.d}, z15\.d
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (max_single_z0_z0_z15, svfloat64x4_t, svfloat64_t,
		    z0 = svmax_single_f64_x4 (z0, z15),
		    z0 = svmax (z0, z15))

/*
** max_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	fmax	{z24\.d - z27\.d}, {z24\.d - z27\.d}, \1\.d
**	ret
*/
TEST_XN_SINGLE (max_single_z24_z24_z16, svfloat64x4_t, svfloat64_t, z24,
		svmax_single_f64_x4 (z24, z16),
		svmax (z24, z16))
