/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** clamp_z24_z24_z0_z5:
**	uclamp	{z24\.b - z27\.b}, z0\.b, z5\.b
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z24_z0_z5, svuint8x4_t, svuint8_t, z24,
		svclamp_single_u8_x4 (z24, z0, z5),
		svclamp (z24, z0, z5))

/*
** clamp_z24_z28_z5_z7:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uclamp	{z24\.b - z27\.b}, z5\.b, z7\.b
** |
**	uclamp	{z28\.b - z31\.b}, z5\.b, z7\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z28_z5_z7, svuint8x4_t, svuint8_t, z24,
		svclamp_single_u8_x4 (z28, z5, z7),
		svclamp (z28, z5, z7))

/*
** clamp_z24_z1_z7_z16:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uclamp	{z24\.b - z27\.b}, z7\.b, z16\.b
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z1_z7_z16, svuint8x4_t, svuint8_t, z24,
		svclamp_single_u8_x4 (z1, z7, z16),
		svclamp (z1, z7, z16))

/*
** clamp_z1_z24_z16_z23:
**	uclamp	{z24\.b - z27\.b}, z16\.b, z23\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z24_z16_z23, svuint8x4_t, svuint8_t, z1,
		svclamp_single_u8_x4 (z24, z16, z23),
		svclamp (z24, z16, z23))

/*
** clamp_z1_z1_z23_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uclamp	{z[0-9]+\.b - z[0-9]+\.b}, z23\.b, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z1_z23_z0, svuint8x4_t, svuint8_t, z1,
		svclamp_single_u8_x4 (z1, z23, z0),
		svclamp (z1, z23, z0))

/*
** clamp_z18_z18_z16_z5:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uclamp	{z[0-9]+\.b - z[0-9]+\.b}, z16\.b, z5\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z18_z18_z16_z5, svuint8x4_t, svuint8_t, z18,
		svclamp_single_u8_x4 (z18, z16, z5),
		svclamp (z18, z16, z5))

/*
** clamp_awkward:
**	...
**	uclamp	{z[0-9]+\.b - z[0-9]+\.b}, z[0-9]+\.b, z5\.b
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (clamp_awkward, svuint8x4_t, svuint8_t,
			z0_res = svclamp_single_u8_x4 (z1, z0, zn),
			z0_res = svclamp (z1, z0, zn))
