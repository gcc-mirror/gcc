/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** rshl_z0_z0_z4:
**	srshl	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (rshl_z0_z0_z4, svint32x2_t, z0,
	 svrshl_s32_x2 (z0, z4),
	 svrshl (z0, z4))

/*
** rshl_z0_z4_z0:
**	srshl	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (rshl_z0_z4_z0, svint32x2_t, z0,
	 svrshl_s32_x2 (z4, z0),
	 svrshl (z4, z0))

/*
** rshl_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	srshl	[^\n]+, {z28\.s - z29\.s}
** |
**	srshl	[^\n]+, {z28\.s - z29\.s}
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (rshl_z0_z4_z28, svint32x2_t, z0,
	 svrshl_s32_x2 (z4, z28),
	 svrshl (z4, z28))

/*
** rshl_z18_z18_z4:
**	srshl	{z18\.s - z19\.s}, {z18\.s - z19\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (rshl_z18_z18_z4, svint32x2_t, z18,
	 svrshl_s32_x2 (z18, z4),
	 svrshl (z18, z4))

/*
** rshl_z23_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	srshl	[^\n]+, {z18\.s - z19\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rshl_z23_z23_z18, svint32x2_t, z23,
	 svrshl_s32_x2 (z23, z18),
	 svrshl (z23, z18))

/*
** rshl_z28_z28_z0:
**	srshl	{z28\.s - z29\.s}, {z28\.s - z29\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (rshl_z28_z28_z0, svint32x2_t, z28,
	 svrshl_s32_x2 (z28, z0),
	 svrshl (z28, z0))

/*
** rshl_z0_z0_z18:
**	srshl	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z18\.s - z19\.s}
**	ret
*/
TEST_XN (rshl_z0_z0_z18, svint32x2_t, z0,
	 svrshl_s32_x2 (z0, z18),
	 svrshl (z0, z18))

/*
** rshl_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	srshl	{z4\.s - z5\.s}, {z4\.s - z5\.s}, [^\n]+
** |
**	srshl	{z4\.s - z5\.s}, {z4\.s - z5\.s}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (rshl_z4_z4_z23, svint32x2_t, z4,
	 svrshl_s32_x2 (z4, z23),
	 svrshl (z4, z23))

/*
** rshl_single_z24_z24_z0:
**	srshl	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z24_z0, svint32x2_t, svint32_t, z24,
		svrshl_single_s32_x2 (z24, z0),
		svrshl (z24, z0))

/*
** rshl_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	srshl	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
** |
**	srshl	{z28\.s - z29\.s}, {z28\.s - z29\.s}, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z28_z0, svint32x2_t, svint32_t, z24,
		svrshl_single_s32_x2 (z28, z0),
		svrshl (z28, z0))

/*
** rshl_single_z24_z1_z0:
** (
**	mov	z24\.d, z1\.d
**	mov	z25\.d, z2\.d
** |
**	mov	z25\.d, z2\.d
**	mov	z24\.d, z1\.d
** )
**	srshl	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z1_z0, svint32x2_t, svint32_t, z24,
		svrshl_single_s32_x2 (z1, z0),
		svrshl (z1, z0))

/*
** rshl_single_z1_z24_z0:
**	srshl	{z24\.s - z25\.s}, {z24\.s - z25\.s}, z0\.s
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (rshl_single_z1_z24_z0, svint32x2_t, svint32_t, z1,
		svrshl_single_s32_x2 (z24, z0),
		svrshl (z24, z0))

/*
** rshl_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	srshl	({z[0-9]+\.s - z[0-9]+\.s}), \1, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (rshl_single_z1_z1_z0, svint32x2_t, svint32_t, z1,
		svrshl_single_s32_x2 (z1, z0),
		svrshl (z1, z0))

/*
** rshl_single_z18_z18_z0:
**	srshl	{z18\.s - z19\.s}, {z18\.s - z19\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (rshl_single_z18_z18_z0, svint32x2_t, svint32_t, z18,
		svrshl_single_s32_x2 (z18, z0),
		svrshl (z18, z0))

/*
** rshl_single_awkward:
**	...
**	srshl	({z[0-9]+\.s - z[0-9]+\.s}), \1, z[0-9]+\.s
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (rshl_single_awkward, svint32x2_t, svint32_t,
			z0_res = svrshl_single_s32_x2 (z1, z0),
			z0_res = svrshl (z1, z0))

/*
** rshl_single_z0_z0_z15:
**	...
**	srshl	{z0\.s - z1\.s}, {z0\.s - z1\.s}, z15\.s
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (rshl_single_z0_z0_z15, svint32x2_t, svint32_t,
		    z0 = svrshl_single_s32_x2 (z0, z15),
		    z0 = svrshl (z0, z15))

/*
** rshl_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	srshl	{z24\.s - z25\.s}, {z24\.s - z25\.s}, \1\.s
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z24_z16, svint32x2_t, svint32_t, z24,
		svrshl_single_s32_x2 (z24, z16),
		svrshl (z24, z16))
