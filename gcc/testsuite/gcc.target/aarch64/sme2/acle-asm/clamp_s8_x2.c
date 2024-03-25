/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** clamp_z24_z24_z0_z5:
**	sclamp	{z24\.b - z25\.b}, z0\.b, z5\.b
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z24_z0_z5, svint8x2_t, svint8_t, z24,
		svclamp_single_s8_x2 (z24, z0, z5),
		svclamp (z24, z0, z5))

/*
** clamp_z24_z28_z5_z7:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	sclamp	{z24\.b - z25\.b}, z5\.b, z7\.b
** |
**	sclamp	{z28\.b - z29\.b}, z5\.b, z7\.b
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z28_z5_z7, svint8x2_t, svint8_t, z24,
		svclamp_single_s8_x2 (z28, z5, z7),
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
**	sclamp	{z24\.b - z25\.b}, z7\.b, z16\.b
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z1_z7_z16, svint8x2_t, svint8_t, z24,
		svclamp_single_s8_x2 (z1, z7, z16),
		svclamp (z1, z7, z16))

/*
** clamp_z1_z24_z16_z23:
**	sclamp	{z24\.b - z25\.b}, z16\.b, z23\.b
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z24_z16_z23, svint8x2_t, svint8_t, z1,
		svclamp_single_s8_x2 (z24, z16, z23),
		svclamp (z24, z16, z23))

/*
** clamp_z1_z1_z23_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	sclamp	{z[0-9]+\.b - z[0-9]+\.b}, z23\.b, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z1_z23_z0, svint8x2_t, svint8_t, z1,
		svclamp_single_s8_x2 (z1, z23, z0),
		svclamp (z1, z23, z0))

/*
** clamp_z18_z18_z0_z23:
**	sclamp	{z18\.b - z19\.b}, z0\.b, z23\.b
**	ret
*/
TEST_XN_SINGLE (clamp_z18_z18_z0_z23, svint8x2_t, svint8_t, z18,
		svclamp_single_s8_x2 (z18, z0, z23),
		svclamp (z18, z0, z23))

/*
** clamp_awkward:
**	...
**	sclamp	{z[0-9]+\.b - z[0-9]+\.b}, z[0-9]+\.b, z3\.b
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (clamp_awkward, svint8x2_t, svint8_t,
			z0_res = svclamp_single_s8_x2 (z1, z0, zn),
			z0_res = svclamp (z1, z0, zn))
