/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+fp8"

/*
** scale_z0_z0_z4:
**	fscale	{z0\.h - z1\.h}, {z0\.h - z1\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_DUAL_XN (scale_z0_z0_z4, svfloat16x2_t, svint16x2_t, z0,
	 svscale_f16_x2 (z0, z4),
	 svscale (z0, z4))

/*
** scale_z4_z4_z0:
**	fscale	{z4\.h - z5\.h}, {z4\.h - z5\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_DUAL_XN (scale_z4_z4_z0, svint16x2_t, svfloat16x2_t, z4,
	 svscale_f16_x2 (z4, z0),
	 svscale (z4, z0))

/*
** scale_z18_z18_z4:
**	fscale	{z18\.h - z19\.h}, {z18\.h - z19\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_DUAL_XN (scale_z18_z18_z4, svfloat16x2_t, svint16x2_t, z18,
	 svscale_f16_x2 (z18, z4),
	 svscale (z18, z4))

/*
** scale_z23_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	[^\n]+, {z18\.h - z19\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (scale_z23_z23_z18, svint16x2_t, svfloat16x2_t, z23,
	 svscale_f16_x2 (z23, z18),
	 svscale (z23, z18))


/*
** scale_z28_z28_z4:
**	fscale	{z28\.h - z29\.h}, {z28\.h - z29\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_DUAL_XN (scale_z28_z28_z4, svfloat16x2_t, svint16x2_t, z28,
	 svscale_f16_x2 (z28, z4),
	 svscale (z28, z4))

/*
** scale_z4_z4_z18:
**	fscale	{z4\.h - z5\.h}, {z4\.h - z5\.h}, {z18\.h - z19\.h}
**	ret
*/
TEST_DUAL_XN (scale_z4_z4_z18, svint16x2_t, svfloat16x2_t, z4,
	 svscale_f16_x2 (z4, z18),
	 svscale (z4, z18))

/*
** scale_z28_28_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	{z28\.h - z29\.h}, {z28\.h - z29\.h}, [^\n]+
** |
**	fscale	{z28\.h - z29\.h}, {z28\.h - z29\.h}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_DUAL_XN (scale_z28_28_z23, svfloat16x2_t, svint16x2_t, z28,
	 svscale_f16_x2 (z28, z23),
	 svscale (z28, z23))

/*
** scale_single_z24_z24_z0:
**	fscale	{z24\.h - z25\.h}, {z24\.h - z25\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (scale_single_z24_z24_z0, svfloat16x2_t, svint16_t, z24,
		svscale_single_f16_x2 (z24, z0),
		svscale (z24, z0))

/*
** scale_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	{z24\.h - z25\.h}, {z24\.h - z25\.h}, z0\.h
** |
**	fscale	{z28\.h - z29\.h}, {z28\.h - z29\.h}, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (scale_single_z24_z28_z0, svfloat16x2_t, svint16_t, z24,
		svscale_single_f16_x2 (z28, z0),
		svscale (z28, z0))

/*
** scale_single_z24_z1_z0:
** (
**	mov	z24\.d, z1\.d
**	mov	z25\.d, z2\.d
** |
**	mov	z25\.d, z2\.d
**	mov	z24\.d, z1\.d
** )
**	fscale	{z24\.h - z25\.h}, {z24\.h - z25\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (scale_single_z24_z1_z0, svfloat16x2_t, svint16_t, z24,
		svscale_single_f16_x2 (z1, z0),
		svscale (z1, z0))

/*
** scale_single_z1_z24_z0:
**	fscale	{z24\.h - z25\.h}, {z24\.h - z25\.h}, z0\.h
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (scale_single_z1_z24_z0, svfloat16x2_t, svint16_t, z1,
		svscale_single_f16_x2 (z24, z0),
		svscale (z24, z0))

/*
** scale_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	({z[0-9]+\.h - z[0-9]+\.h}), \1, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (scale_single_z1_z1_z0, svfloat16x2_t, svint16_t, z1,
		svscale_single_f16_x2 (z1, z0),
		svscale (z1, z0))

/*
** scale_single_z18_z18_z0:
**	fscale	{z18\.h - z19\.h}, {z18\.h - z19\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (scale_single_z18_z18_z0, svfloat16x2_t, svint16_t, z18,
		svscale_single_f16_x2 (z18, z0),
		svscale (z18, z0))

/*
** scale_single_awkward:
**	...
**	fscale	({z[0-9]+\.h - z[0-9]+\.h}), \1, z[0-9]+\.h
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (scale_single_awkward, svfloat16x2_t, svint16_t,
			z0_res = svscale_single_f16_x2 (z1, z0),
			z0_res = svscale (z1, z0))

/*
** scale_single_z0_z0_z15:
**	...
**	fscale	{z0\.h - z1\.h}, {z0\.h - z1\.h}, z15\.h
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (scale_single_z0_z0_z15, svfloat16x2_t, svint16_t,
		    z0 = svscale_single_f16_x2 (z0, z15),
		    z0 = svscale (z0, z15))

/*
** scale_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	fscale	{z24\.h - z25\.h}, {z24\.h - z25\.h}, \1\.h
**	ret
*/
TEST_XN_SINGLE (scale_single_z24_z24_z16, svfloat16x2_t, svint16_t, z24,
		svscale_single_f16_x2 (z24, z16),
		svscale (z24, z16))
