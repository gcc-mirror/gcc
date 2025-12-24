/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+fp8"

/*
** fscale_z0_z0_z4:
**	fscale	{z0\.h - z3\.h}, {z0\.h - z3\.h}, {z4\.h - z7\.h}
**	ret
*/
TEST_DUAL_XN (fscale_z0_z0_z4, svfloat16x4_t, svint16x4_t, z0,
	      svscale_f16_x4 (z0, z4),
	      svscale (z0, z4))

/*
** fscale_z4_z4_z0:
**	fscale	{z4\.h - z7\.h}, {z4\.h - z7\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_DUAL_XN (fscale_z4_z4_z0, svint16x4_t, svfloat16x4_t, z4,
	      svscale_f16_x4 (z4, z0),
	      svscale (z4, z0))

/*
** fscale_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	[^\n]+, {z4\.h - z7\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (fscale_z18_z18_z4, svfloat16x4_t, svint16x4_t, z18,
	      svscale_f16_x4 (z18, z4),
	      svscale (z18, z4))

/*
** fscale_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	[^\n]+, {z28\.h - z31\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (fscale_z23_z23_z28, svint16x4_t, svfloat16x4_t, z23,
	      svscale_f16_x4 (z23, z28),
	      svscale (z23, z28))

/*
** fscale_z28_z28_z4:
**	fscale	{z28\.h - z31\.h}, {z28\.h - z31\.h}, {z4\.h - z7\.h}
**	ret
*/
TEST_DUAL_XN (fscale_z28_z28_z4, svfloat16x4_t, svint16x4_t, z28,
	      svscale_f16_x4 (z28, z4),
	      svscale (z28, z4))

/*
** fscale_z4_z4_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	{z4\.h - z7\.h}, {z4\.h - z7\.h}, [^\n]+
** |
**	fscale	{z4\.h - z7\.h}, {z4\.h - z7\.h}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_DUAL_XN (fscale_z4_z4_z18, svint16x4_t, svfloat16x4_t, z4,
	      svscale_f16_x4 (z4, z18),
	      svscale (z4, z18))

/*
** fscale_z0_z0_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	{z0\.h - z3\.h}, {z0\.h - z3\.h}, [^\n]+
** |
**	fscale	{z0\.h - z3\.h}, {z0\.h - z3\.h}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_DUAL_XN (fscale_z0_z0_z23, svfloat16x4_t, svint16x4_t, z0,
	      svscale_f16_x4 (z0, z23),
	      svscale (z0, z23))

/*
** fscale_single_z24_z24_z0:
**	fscale	{z24\.h - z27\.h}, {z24\.h - z27\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (fscale_single_z24_z24_z0, svfloat16x4_t, svint16_t, z24,
		svscale_single_f16_x4 (z24, z0),
		svscale (z24, z0))

/*
** fscale_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	{z24\.h - z27\.h}, {z24\.h - z27\.h}, z0\.h
** |
**	fscale	{z28\.h - z31\.h}, {z28\.h - z31\.h}, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (fscale_single_z24_z28_z0, svfloat16x4_t, svint16_t, z24,
		svscale_single_f16_x4 (z28, z0),
		svscale (z28, z0))

/*
** fscale_single_z24_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	{z24\.h - z27\.h}, {z24\.h - z27\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (fscale_single_z24_z1_z0, svfloat16x4_t, svint16_t, z24,
		svscale_single_f16_x4 (z1, z0),
		svscale (z1, z0))

/*
** fscale_single_z1_z24_z0:
**	fscale	{z24\.h - z27\.h}, {z24\.h - z27\.h}, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (fscale_single_z1_z24_z0, svfloat16x4_t, svint16_t, z1,
		svscale_single_f16_x4 (z24, z0),
		svscale (z24, z0))

/*
** fscale_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	({z[0-9]+\.h - z[0-9]+\.h}), \1, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (fscale_single_z1_z1_z0, svfloat16x4_t, svint16_t, z1,
		svscale_single_f16_x4 (z1, z0),
		svscale (z1, z0))

/*
** fscale_single_z18_z18_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	[^\n]+, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (fscale_single_z18_z18_z0, svfloat16x4_t, svint16_t, z18,
		svscale_single_f16_x4 (z18, z0),
		svscale (z18, z0))

/*
** fscale_single_awkward:
**	...
**	fscale	({z[0-9]+\.h - z[0-9]+\.h}), \1, z[0-9]+\.h
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (fscale_single_awkward, svfloat16x4_t, svint16_t,
			z0_res = svscale_single_f16_x4 (z1, z0),
			z0_res = svscale (z1, z0))

/*
** fscale_single_z0_z0_z15:
**	...
**	fscale	{z0\.h - z3\.h}, {z0\.h - z3\.h}, z15\.h
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (fscale_single_z0_z0_z15, svfloat16x4_t, svint16_t,
		    z0 = svscale_single_f16_x4 (z0, z15),
		    z0 = svscale (z0, z15))

/*
** fscale_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	fscale	{z24\.h - z27\.h}, {z24\.h - z27\.h}, \1\.h
**	ret
*/
TEST_XN_SINGLE (fscale_single_z24_z24_z16, svfloat16x4_t, svint16_t, z24,
		svscale_single_f16_x4 (z24, z16),
		svscale (z24, z16))
