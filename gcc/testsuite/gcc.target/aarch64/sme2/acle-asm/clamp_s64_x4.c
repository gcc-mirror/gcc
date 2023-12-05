/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** clamp_z24_z24_z0_z5:
**	sclamp	{z24\.d - z27\.d}, z0\.d, z5\.d
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z24_z0_z5, svint64x4_t, svint64_t, z24,
		svclamp_single_s64_x4 (z24, z0, z5),
		svclamp (z24, z0, z5))

/*
** clamp_z24_z28_z5_z7:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sclamp	{z24\.d - z27\.d}, z5\.d, z7\.d
** |
**	sclamp	{z28\.d - z31\.d}, z5\.d, z7\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z28_z5_z7, svint64x4_t, svint64_t, z24,
		svclamp_single_s64_x4 (z28, z5, z7),
		svclamp (z28, z5, z7))

/*
** clamp_z24_z1_z7_z16:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sclamp	{z24\.d - z27\.d}, z7\.d, z16\.d
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z1_z7_z16, svint64x4_t, svint64_t, z24,
		svclamp_single_s64_x4 (z1, z7, z16),
		svclamp (z1, z7, z16))

/*
** clamp_z1_z24_z16_z23:
**	sclamp	{z24\.d - z27\.d}, z16\.d, z23\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z24_z16_z23, svint64x4_t, svint64_t, z1,
		svclamp_single_s64_x4 (z24, z16, z23),
		svclamp (z24, z16, z23))

/*
** clamp_z1_z1_z23_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sclamp	{z[0-9]+\.d - z[0-9]+\.d}, z23\.d, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z1_z23_z0, svint64x4_t, svint64_t, z1,
		svclamp_single_s64_x4 (z1, z23, z0),
		svclamp (z1, z23, z0))

/*
** clamp_z18_z18_z16_z5:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sclamp	{z[0-9]+\.d - z[0-9]+\.d}, z16\.d, z5\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z18_z18_z16_z5, svint64x4_t, svint64_t, z18,
		svclamp_single_s64_x4 (z18, z16, z5),
		svclamp (z18, z16, z5))

/*
** clamp_awkward:
**	...
**	sclamp	{z[0-9]+\.d - z[0-9]+\.d}, z[0-9]+\.d, z5\.d
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (clamp_awkward, svint64x4_t, svint64_t,
			z0_res = svclamp_single_s64_x4 (z1, z0, zn),
			z0_res = svclamp (z1, z0, zn))
