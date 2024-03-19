/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** max_z0_z0_z4:
**	fmax	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (max_z0_z0_z4, svfloat32x2_t, z0,
	 svmax_f32_x2 (z0, z4),
	 svmax (z0, z4))

/*
** max_z0_z4_z0:
**	fmax	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (max_z0_z4_z0, svfloat32x2_t, z0,
	 svmax_f32_x2 (z4, z0),
	 svmax (z4, z0))

/*
** max_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	[^\n]+, {z28\.s - z29\.s}
** |
**	fmax	[^\n]+, {z28\.s - z29\.s}
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (max_z0_z4_z28, svfloat32x2_t, z0,
	 svmax_f32_x2 (z4, z28),
	 svmax (z4, z28))

/*
** max_z18_z18_z4:
**	fmax	{z18\.s - z19\.s}, {z18\.s - z19\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (max_z18_z18_z4, svfloat32x2_t, z18,
	 svmax_f32_x2 (z18, z4),
	 svmax (z18, z4))

/*
** max_z23_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	[^\n]+, {z18\.s - z19\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (max_z23_z23_z18, svfloat32x2_t, z23,
	 svmax_f32_x2 (z23, z18),
	 svmax (z23, z18))

/*
** max_z28_z28_z0:
**	fmax	{z28\.s - z29\.s}, {z28\.s - z29\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (max_z28_z28_z0, svfloat32x2_t, z28,
	 svmax_f32_x2 (z28, z0),
	 svmax (z28, z0))

/*
** max_z0_z0_z18:
**	fmax	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z18\.s - z19\.s}
**	ret
*/
TEST_XN (max_z0_z0_z18, svfloat32x2_t, z0,
	 svmax_f32_x2 (z0, z18),
	 svmax (z0, z18))

/*
** max_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	{z4\.s - z5\.s}, {z4\.s - z5\.s}, [^\n]+
** |
**	fmax	{z4\.s - z5\.s}, {z4\.s - z5\.s}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (max_z4_z4_z23, svfloat32x2_t, z4,
	 svmax_f32_x2 (z4, z23),
	 svmax (z4, z23))

/*
** max_single_z24_z24_z0:
**	fmax	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (max_single_z24_z24_z0, svfloat32x2_t, svfloat32_t, z24,
		svmax_single_f32_x2 (z24, z0),
		svmax (z24, z0))

/*
** max_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
** |
**	fmax	{z28\.s - z29\.s}, {z28\.s - z29\.s}, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (max_single_z24_z28_z0, svfloat32x2_t, svfloat32_t, z24,
		svmax_single_f32_x2 (z28, z0),
		svmax (z28, z0))

/*
** max_single_z24_z1_z0:
** (
**	mov	z24\.d, z1\.d
**	mov	z25\.d, z2\.d
** |
**	mov	z25\.d, z2\.d
**	mov	z24\.d, z1\.d
** )
**	fmax	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (max_single_z24_z1_z0, svfloat32x2_t, svfloat32_t, z24,
		svmax_single_f32_x2 (z1, z0),
		svmax (z1, z0))

/*
** max_single_z1_z24_z0:
**	fmax	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (max_single_z1_z24_z0, svfloat32x2_t, svfloat32_t, z1,
		svmax_single_f32_x2 (z24, z0),
		svmax (z24, z0))

/*
** max_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	fmax	({z[0-9]+\.s - z[0-9]+\.s}), \1, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (max_single_z1_z1_z0, svfloat32x2_t, svfloat32_t, z1,
		svmax_single_f32_x2 (z1, z0),
		svmax (z1, z0))

/*
** max_single_z18_z18_z0:
**	fmax	{z18\.s - z19\.s}, {z18\.s - z19\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (max_single_z18_z18_z0, svfloat32x2_t, svfloat32_t, z18,
		svmax_single_f32_x2 (z18, z0),
		svmax (z18, z0))

/*
** max_single_awkward:
**	...
**	fmax	({z[0-9]+\.s - z[0-9]+\.s}), \1, z[0-9]+\.s
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (max_single_awkward, svfloat32x2_t, svfloat32_t,
			z0_res = svmax_single_f32_x2 (z1, z0),
			z0_res = svmax (z1, z0))

/*
** max_single_z0_z0_z15:
**	...
**	fmax	{z0\.s - z1\.s}, {z0\.s - z1\.s}, z15\.s
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (max_single_z0_z0_z15, svfloat32x2_t, svfloat32_t,
		    z0 = svmax_single_f32_x2 (z0, z15),
		    z0 = svmax (z0, z15))

/*
** max_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	fmax	{z24\.s - z25\.s}, {z24\.s - z25\.s}, \1\.s
**	ret
*/
TEST_XN_SINGLE (max_single_z24_z24_z16, svfloat32x2_t, svfloat32_t, z24,
		svmax_single_f32_x2 (z24, z16),
		svmax (z24, z16))
