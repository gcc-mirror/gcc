/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+fp8"

/*
** svscale_z0_z0_z4:
**	fscale	{z0\.d - z3\.d}, {z0\.d - z3\.d}, {z4\.d - z7\.d}
**	ret
*/
TEST_DUAL_XN (svscale_z0_z0_z4, svfloat64x4_t, svint64x4_t, z0,
	      svscale_f64_x4 (z0, z4),
	      svscale (z0, z4))

/*
** svscale_z4_z4_z0:
**	fscale	{z4\.d - z7\.d}, {z4\.d - z7\.d}, {z0\.d - z3\.d}
**	ret
*/
TEST_DUAL_XN (svscale_z4_z4_z0, svint64x4_t, svfloat64x4_t, z4,
	      svscale_f64_x4 (z4, z0),
	      svscale (z4, z0))

/*
** svscale_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	[^\n]+, {z4\.d - z7\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (svscale_z18_z18_z4, svfloat64x4_t, svint64x4_t, z18,
	      svscale_f64_x4 (z18, z4),
	      svscale (z18, z4))

/*
** svscale_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	[^\n]+, {z28\.d - z31\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (svscale_z23_z23_z28, svint64x4_t, svfloat64x4_t, z23,
	      svscale_f64_x4 (z23, z28),
	      svscale (z23, z28))

/*
** svscale_z28_z28_z4:
**	fscale	{z28\.d - z31\.d}, {z28\.d - z31\.d}, {z4\.d - z7\.d}
**	ret
*/
TEST_DUAL_XN (svscale_z28_z28_z4, svfloat64x4_t, svint64x4_t, z28,
	      svscale_f64_x4 (z28, z4),
	      svscale (z28, z4))

/*
** svscale_z4_z4_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	{z4\.d - z7\.d}, {z4\.d - z7\.d}, [^\n]+
** |
**	fscale	{z4\.d - z7\.d}, {z4\.d - z7\.d}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_DUAL_XN (svscale_z4_z4_z18, svint64x4_t, svfloat64x4_t, z4,
	      svscale_f64_x4 (z4, z18),
	      svscale (z4, z18))

/*
** svscale_z0_z0_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	{z0\.d - z3\.d}, {z0\.d - z3\.d}, [^\n]+
** |
**	fscale	{z0\.d - z3\.d}, {z0\.d - z3\.d}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_DUAL_XN (svscale_z0_z0_z23, svfloat64x4_t, svint64x4_t, z0,
	      svscale_f64_x4 (z0, z23),
	      svscale (z0, z23))

/*
** svscale_single_z24_z24_z0:
**	fscale	{z24\.d - z27\.d}, {z24\.d - z27\.d}, z0\.d
**	ret
*/
TEST_XN_SINGLE (svscale_single_z24_z24_z0, svfloat64x4_t, svint64_t, z24,
		svscale_single_f64_x4 (z24, z0),
		svscale (z24, z0))

/*
** svscale_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	{z24\.d - z27\.d}, {z24\.d - z27\.d}, z0\.d
** |
**	fscale	{z28\.d - z31\.d}, {z28\.d - z31\.d}, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (svscale_single_z24_z28_z0, svfloat64x4_t, svint64_t, z24,
		svscale_single_f64_x4 (z28, z0),
		svscale (z28, z0))

/*
** svscale_single_z24_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	{z24\.d - z27\.d}, {z24\.d - z27\.d}, z0\.d
**	ret
*/
TEST_XN_SINGLE (svscale_single_z24_z1_z0, svfloat64x4_t, svint64_t, z24,
		svscale_single_f64_x4 (z1, z0),
		svscale (z1, z0))

/*
** svscale_single_z1_z24_z0:
**	fscale	{z24\.d - z27\.d}, {z24\.d - z27\.d}, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (svscale_single_z1_z24_z0, svfloat64x4_t, svint64_t, z1,
		svscale_single_f64_x4 (z24, z0),
		svscale (z24, z0))

/*
** svscale_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	({z[0-9]+\.d - z[0-9]+\.d}), \1, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (svscale_single_z1_z1_z0, svfloat64x4_t, svint64_t, z1,
		svscale_single_f64_x4 (z1, z0),
		svscale (z1, z0))

/*
** svscale_single_z18_z18_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	[^\n]+, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (svscale_single_z18_z18_z0, svfloat64x4_t, svint64_t, z18,
		svscale_single_f64_x4 (z18, z0),
		svscale (z18, z0))

/*
** svscale_single_awkward:
**	...
**	fscale	({z[0-9]+\.d - z[0-9]+\.d}), \1, z[0-9]+\.d
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (svscale_single_awkward, svfloat64x4_t, svint64_t,
			z0_res = svscale_single_f64_x4 (z1, z0),
			z0_res = svscale (z1, z0))

/*
** svscale_single_z0_z0_z15:
**	...
**	fscale	{z0\.d - z3\.d}, {z0\.d - z3\.d}, z15\.d
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (svscale_single_z0_z0_z15, svfloat64x4_t, svint64_t,
		    z0 = svscale_single_f64_x4 (z0, z15),
		    z0 = svscale (z0, z15))

/*
** svscale_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	fscale	{z24\.d - z27\.d}, {z24\.d - z27\.d}, \1\.d
**	ret
*/
TEST_XN_SINGLE (svscale_single_z24_z24_z16, svfloat64x4_t, svint64_t, z24,
		svscale_single_f64_x4 (z24, z16),
		svscale (z24, z16))
