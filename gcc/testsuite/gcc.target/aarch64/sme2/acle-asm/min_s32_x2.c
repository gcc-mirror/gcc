/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** min_z0_z0_z4:
**	smin	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (min_z0_z0_z4, svint32x2_t, z0,
	 svmin_s32_x2 (z0, z4),
	 svmin (z0, z4))

/*
** min_z0_z4_z0:
**	smin	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (min_z0_z4_z0, svint32x2_t, z0,
	 svmin_s32_x2 (z4, z0),
	 svmin (z4, z0))

/*
** min_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	smin	[^\n]+, {z28\.s - z29\.s}
** |
**	smin	[^\n]+, {z28\.s - z29\.s}
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (min_z0_z4_z28, svint32x2_t, z0,
	 svmin_s32_x2 (z4, z28),
	 svmin (z4, z28))

/*
** min_z18_z18_z4:
**	smin	{z18\.s - z19\.s}, {z18\.s - z19\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (min_z18_z18_z4, svint32x2_t, z18,
	 svmin_s32_x2 (z18, z4),
	 svmin (z18, z4))

/*
** min_z23_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	smin	[^\n]+, {z18\.s - z19\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (min_z23_z23_z18, svint32x2_t, z23,
	 svmin_s32_x2 (z23, z18),
	 svmin (z23, z18))

/*
** min_z28_z28_z0:
**	smin	{z28\.s - z29\.s}, {z28\.s - z29\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (min_z28_z28_z0, svint32x2_t, z28,
	 svmin_s32_x2 (z28, z0),
	 svmin (z28, z0))

/*
** min_z0_z0_z18:
**	smin	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z18\.s - z19\.s}
**	ret
*/
TEST_XN (min_z0_z0_z18, svint32x2_t, z0,
	 svmin_s32_x2 (z0, z18),
	 svmin (z0, z18))

/*
** min_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	smin	{z4\.s - z5\.s}, {z4\.s - z5\.s}, [^\n]+
** |
**	smin	{z4\.s - z5\.s}, {z4\.s - z5\.s}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (min_z4_z4_z23, svint32x2_t, z4,
	 svmin_s32_x2 (z4, z23),
	 svmin (z4, z23))

/*
** min_single_z24_z24_z0:
**	smin	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (min_single_z24_z24_z0, svint32x2_t, svint32_t, z24,
		svmin_single_s32_x2 (z24, z0),
		svmin (z24, z0))

/*
** min_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	smin	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
** |
**	smin	{z28\.s - z29\.s}, {z28\.s - z29\.s}, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (min_single_z24_z28_z0, svint32x2_t, svint32_t, z24,
		svmin_single_s32_x2 (z28, z0),
		svmin (z28, z0))

/*
** min_single_z24_z1_z0:
** (
**	mov	z24\.d, z1\.d
**	mov	z25\.d, z2\.d
** |
**	mov	z25\.d, z2\.d
**	mov	z24\.d, z1\.d
** )
**	smin	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (min_single_z24_z1_z0, svint32x2_t, svint32_t, z24,
		svmin_single_s32_x2 (z1, z0),
		svmin (z1, z0))

/*
** min_single_z1_z24_z0:
**	smin	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (min_single_z1_z24_z0, svint32x2_t, svint32_t, z1,
		svmin_single_s32_x2 (z24, z0),
		svmin (z24, z0))

/*
** min_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	smin	({z[0-9]+\.s - z[0-9]+\.s}), \1, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (min_single_z1_z1_z0, svint32x2_t, svint32_t, z1,
		svmin_single_s32_x2 (z1, z0),
		svmin (z1, z0))

/*
** min_single_z18_z18_z0:
**	smin	{z18\.s - z19\.s}, {z18\.s - z19\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (min_single_z18_z18_z0, svint32x2_t, svint32_t, z18,
		svmin_single_s32_x2 (z18, z0),
		svmin (z18, z0))

/*
** min_single_awkward:
**	...
**	smin	({z[0-9]+\.s - z[0-9]+\.s}), \1, z[0-9]+\.s
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (min_single_awkward, svint32x2_t, svint32_t,
			z0_res = svmin_single_s32_x2 (z1, z0),
			z0_res = svmin (z1, z0))

/*
** min_single_z0_z0_z15:
**	...
**	smin	{z0\.s - z1\.s}, {z0\.s - z1\.s}, z15\.s
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (min_single_z0_z0_z15, svint32x2_t, svint32_t,
		    z0 = svmin_single_s32_x2 (z0, z15),
		    z0 = svmin (z0, z15))

/*
** min_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	smin	{z24\.s - z25\.s}, {z24\.s - z25\.s}, \1\.s
**	ret
*/
TEST_XN_SINGLE (min_single_z24_z24_z16, svint32x2_t, svint32_t, z24,
		svmin_single_s32_x2 (z24, z16),
		svmin (z24, z16))
