/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */
/* { dg-do assemble { target { aarch64_asm_fp8_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_fp8_ok } } } } */

#include "test_sme2_acle.h"
#pragma GCC target "+fp8"

/*
** svscale_z0_z0_z4:
**	fscale	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_DUAL_XN (svscale_z0_z0_z4, svfloat32x2_t, svint32x2_t, z0,
	      svscale_f32_x2 (z0, z4),
	      svscale (z0, z4))

/*
** svscale_z4_z4_z0:
**	fscale	{z4\.s - z5\.s}, {z4\.s - z5\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_DUAL_XN (svscale_z4_z4_z0, svint32x2_t, svfloat32x2_t, z4,
	      svscale_f32_x2 (z4, z0),
	      svscale (z4, z0))

/*
** svscale_z0_z28_z4:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	[^\n]+, {z4\.s - z5\.s}
** |
**	fscale	[^\n]+, {z4\.s - z5\.s}
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_DUAL_XN (svscale_z0_z28_z4, svfloat32x2_t, svint32x2_t, z0,
	      svscale_f32_x2 (z28, z4),
	      svscale (z28, z4))

/*
** svscale_z18_z18_z4:
**	fscale	{z18\.s - z19\.s}, {z18\.s - z19\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_DUAL_XN (svscale_z18_z18_z4, svfloat32x2_t, svint32x2_t, z18,
	      svscale_f32_x2 (z18, z4),
	      svscale (z18, z4))

/*
** svscale_z23_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	[^\n]+, {z18\.s - z19\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (svscale_z23_z23_z18, svint32x2_t, svfloat32x2_t, z23,
	      svscale_f32_x2 (z23, z18),
	      svscale (z23, z18))

/*
** svscale_z28_z28_z4:
**	fscale	{z28\.s - z29\.s}, {z28\.s - z29\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_DUAL_XN (svscale_z28_z28_z4, svfloat32x2_t, svint32x2_t, z28,
	      svscale_f32_x2 (z28, z4),
	      svscale (z28, z4))

/*
** svscale_z4_z4_z18:
**	fscale	{z4\.s - z5\.s}, {z4\.s - z5\.s}, {z18\.s - z19\.s}
**	ret
*/
TEST_DUAL_XN (svscale_z4_z4_z18, svint32x2_t, svfloat32x2_t, z4,
	      svscale_f32_x2 (z4, z18),
	      svscale (z4, z18))

/*
** svscale_z28_z28_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	{z28\.s - z29\.s}, {z28\.s - z29\.s}, [^\n]+
** |
**	fscale	{z28\.s - z29\.s}, {z28\.s - z29\.s}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_DUAL_XN (svscale_z28_z28_z23, svfloat32x2_t, svint32x2_t, z28,
	      svscale_f32_x2 (z28, z23),
	      svscale (z28, z23))

/*
** svscale_single_z24_z24_z0:
**	fscale	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (svscale_single_z24_z24_z0, svfloat32x2_t, svint32_t, z24,
		svscale_single_f32_x2 (z24, z0),
		svscale (z24, z0))

/*
** svscale_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
** |
**	fscale	{z28\.s - z29\.s}, {z28\.s - z29\.s}, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (svscale_single_z24_z28_z0, svfloat32x2_t, svint32_t, z24,
		svscale_single_f32_x2 (z28, z0),
		svscale (z28, z0))

/*
** svscale_single_z24_z1_z0:
** (
**	mov	z24\.d, z1\.d
**	mov	z25\.d, z2\.d
** |
**	mov	z25\.d, z2\.d
**	mov	z24\.d, z1\.d
** )
**	fscale	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (svscale_single_z24_z1_z0, svfloat32x2_t, svint32_t, z24,
		svscale_single_f32_x2 (z1, z0),
		svscale (z1, z0))

/*
** svscale_single_z1_z24_z0:
**	fscale	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (svscale_single_z1_z24_z0, svfloat32x2_t, svint32_t, z1,
		svscale_single_f32_x2 (z24, z0),
		svscale (z24, z0))

/*
** svscale_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	fscale	({z[0-9]+\.s - z[0-9]+\.s}), \1, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (svscale_single_z1_z1_z0, svfloat32x2_t, svint32_t, z1,
		svscale_single_f32_x2 (z1, z0),
		svscale (z1, z0))

/*
** svscale_single_z18_z18_z0:
**	fscale	{z18\.s - z19\.s}, {z18\.s - z19\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (svscale_single_z18_z18_z0, svfloat32x2_t, svint32_t, z18,
		svscale_single_f32_x2 (z18, z0),
		svscale (z18, z0))

/*
** svscale_single_awkward:
**	...
**	fscale	({z[0-9]+\.s - z[0-9]+\.s}), \1, z[0-9]+\.s
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (svscale_single_awkward, svfloat32x2_t, svint32_t,
			z0_res = svscale_single_f32_x2 (z1, z0),
			z0_res = svscale (z1, z0))

/*
** svscale_single_z0_z0_z15:
**	...
**	fscale	{z0\.s - z1\.s}, {z0\.s - z1\.s}, z15\.s
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (svscale_single_z0_z0_z15, svfloat32x2_t, svint32_t,
		    z0 = svscale_single_f32_x2 (z0, z15),
		    z0 = svscale (z0, z15))

/*
** svscale_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	fscale	{z24\.s - z25\.s}, {z24\.s - z25\.s}, \1\.s
**	ret
*/
TEST_XN_SINGLE (svscale_single_z24_z24_z16, svfloat32x2_t, svint32_t, z24,
		svscale_single_f32_x2 (z24, z16),
		svscale (z24, z16))
