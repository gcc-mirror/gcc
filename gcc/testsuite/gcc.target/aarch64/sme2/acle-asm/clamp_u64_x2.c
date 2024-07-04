/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** clamp_z24_z24_z0_z5:
**	uclamp	{z24\.d - z25\.d}, z0\.d, z5\.d
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z24_z0_z5, svuint64x2_t, svuint64_t, z24,
		svclamp_single_u64_x2 (z24, z0, z5),
		svclamp (z24, z0, z5))

/*
** clamp_z24_z28_z5_z7:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	uclamp	{z24\.d - z25\.d}, z5\.d, z7\.d
** |
**	uclamp	{z28\.d - z29\.d}, z5\.d, z7\.d
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z28_z5_z7, svuint64x2_t, svuint64_t, z24,
		svclamp_single_u64_x2 (z28, z5, z7),
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
**	uclamp	{z24\.d - z25\.d}, z7\.d, z16\.d
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z1_z7_z16, svuint64x2_t, svuint64_t, z24,
		svclamp_single_u64_x2 (z1, z7, z16),
		svclamp (z1, z7, z16))

/*
** clamp_z1_z24_z16_z23:
**	uclamp	{z24\.d - z25\.d}, z16\.d, z23\.d
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z24_z16_z23, svuint64x2_t, svuint64_t, z1,
		svclamp_single_u64_x2 (z24, z16, z23),
		svclamp (z24, z16, z23))

/*
** clamp_z1_z1_z23_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	uclamp	{z[0-9]+\.d - z[0-9]+\.d}, z23\.d, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z1_z23_z0, svuint64x2_t, svuint64_t, z1,
		svclamp_single_u64_x2 (z1, z23, z0),
		svclamp (z1, z23, z0))

/*
** clamp_z18_z18_z0_z23:
**	uclamp	{z18\.d - z19\.d}, z0\.d, z23\.d
**	ret
*/
TEST_XN_SINGLE (clamp_z18_z18_z0_z23, svuint64x2_t, svuint64_t, z18,
		svclamp_single_u64_x2 (z18, z0, z23),
		svclamp (z18, z0, z23))

/*
** clamp_awkward:
**	...
**	uclamp	{z[0-9]+\.d - z[0-9]+\.d}, z[0-9]+\.d, z3\.d
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (clamp_awkward, svuint64x2_t, svuint64_t,
			z0_res = svclamp_single_u64_x2 (z1, z0, zn),
			z0_res = svclamp (z1, z0, zn))
