/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** clamp_z24_z24_z0_z5:
**	uclamp	{z24\.s - z27\.s}, z0\.s, z5\.s
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z24_z0_z5, svuint32x4_t, svuint32_t, z24,
		svclamp_single_u32_x4 (z24, z0, z5),
		svclamp (z24, z0, z5))

/*
** clamp_z24_z28_z5_z7:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uclamp	{z24\.s - z27\.s}, z5\.s, z7\.s
** |
**	uclamp	{z28\.s - z31\.s}, z5\.s, z7\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z28_z5_z7, svuint32x4_t, svuint32_t, z24,
		svclamp_single_u32_x4 (z28, z5, z7),
		svclamp (z28, z5, z7))

/*
** clamp_z24_z1_z7_z16:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uclamp	{z24\.s - z27\.s}, z7\.s, z16\.s
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z1_z7_z16, svuint32x4_t, svuint32_t, z24,
		svclamp_single_u32_x4 (z1, z7, z16),
		svclamp (z1, z7, z16))

/*
** clamp_z1_z24_z16_z23:
**	uclamp	{z24\.s - z27\.s}, z16\.s, z23\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z24_z16_z23, svuint32x4_t, svuint32_t, z1,
		svclamp_single_u32_x4 (z24, z16, z23),
		svclamp (z24, z16, z23))

/*
** clamp_z1_z1_z23_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uclamp	{z[0-9]+\.s - z[0-9]+\.s}, z23\.s, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z1_z23_z0, svuint32x4_t, svuint32_t, z1,
		svclamp_single_u32_x4 (z1, z23, z0),
		svclamp (z1, z23, z0))

/*
** clamp_z18_z18_z16_z5:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uclamp	{z[0-9]+\.s - z[0-9]+\.s}, z16\.s, z5\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z18_z18_z16_z5, svuint32x4_t, svuint32_t, z18,
		svclamp_single_u32_x4 (z18, z16, z5),
		svclamp (z18, z16, z5))

/*
** clamp_awkward:
**	...
**	uclamp	{z[0-9]+\.s - z[0-9]+\.s}, z[0-9]+\.s, z5\.s
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (clamp_awkward, svuint32x4_t, svuint32_t,
			z0_res = svclamp_single_u32_x4 (z1, z0, zn),
			z0_res = svclamp (z1, z0, zn))
