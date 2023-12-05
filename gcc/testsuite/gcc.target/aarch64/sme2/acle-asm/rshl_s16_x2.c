/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** rshl_z0_z0_z4:
**	srshl	{z0\.h - z1\.h}, {z0\.h - z1\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_XN (rshl_z0_z0_z4, svint16x2_t, z0,
	 svrshl_s16_x2 (z0, z4),
	 svrshl (z0, z4))

/*
** rshl_z0_z4_z0:
**	srshl	{z0\.h - z1\.h}, {z0\.h - z1\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_XN (rshl_z0_z4_z0, svint16x2_t, z0,
	 svrshl_s16_x2 (z4, z0),
	 svrshl (z4, z0))

/*
** rshl_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	srshl	[^\n]+, {z28\.h - z29\.h}
** |
**	srshl	[^\n]+, {z28\.h - z29\.h}
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (rshl_z0_z4_z28, svint16x2_t, z0,
	 svrshl_s16_x2 (z4, z28),
	 svrshl (z4, z28))

/*
** rshl_z18_z18_z4:
**	srshl	{z18\.h - z19\.h}, {z18\.h - z19\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_XN (rshl_z18_z18_z4, svint16x2_t, z18,
	 svrshl_s16_x2 (z18, z4),
	 svrshl (z18, z4))

/*
** rshl_z23_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	srshl	[^\n]+, {z18\.h - z19\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (rshl_z23_z23_z18, svint16x2_t, z23,
	 svrshl_s16_x2 (z23, z18),
	 svrshl (z23, z18))

/*
** rshl_z28_z28_z0:
**	srshl	{z28\.h - z29\.h}, {z28\.h - z29\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_XN (rshl_z28_z28_z0, svint16x2_t, z28,
	 svrshl_s16_x2 (z28, z0),
	 svrshl (z28, z0))

/*
** rshl_z0_z0_z18:
**	srshl	{z0\.h - z1\.h}, {z0\.h - z1\.h}, {z18\.h - z19\.h}
**	ret
*/
TEST_XN (rshl_z0_z0_z18, svint16x2_t, z0,
	 svrshl_s16_x2 (z0, z18),
	 svrshl (z0, z18))

/*
** rshl_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	srshl	{z4\.h - z5\.h}, {z4\.h - z5\.h}, [^\n]+
** |
**	srshl	{z4\.h - z5\.h}, {z4\.h - z5\.h}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (rshl_z4_z4_z23, svint16x2_t, z4,
	 svrshl_s16_x2 (z4, z23),
	 svrshl (z4, z23))

/*
** rshl_single_z24_z24_z0:
**	srshl	{z24\.h - z25\.h}, {z24\.h - z25\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z24_z0, svint16x2_t, svint16_t, z24,
		svrshl_single_s16_x2 (z24, z0),
		svrshl (z24, z0))

/*
** rshl_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	srshl	{z24\.h - z25\.h}, {z24\.h - z25\.h}, z0\.h
** |
**	srshl	{z28\.h - z29\.h}, {z28\.h - z29\.h}, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z28_z0, svint16x2_t, svint16_t, z24,
		svrshl_single_s16_x2 (z28, z0),
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
**	srshl	{z24\.h - z25\.h}, {z24\.h - z25\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z1_z0, svint16x2_t, svint16_t, z24,
		svrshl_single_s16_x2 (z1, z0),
		svrshl (z1, z0))

/*
** rshl_single_z1_z24_z0:
**	srshl	{z24\.h - z25\.h}, {z24\.h - z25\.h}, z0\.h
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (rshl_single_z1_z24_z0, svint16x2_t, svint16_t, z1,
		svrshl_single_s16_x2 (z24, z0),
		svrshl (z24, z0))

/*
** rshl_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	srshl	({z[0-9]+\.h - z[0-9]+\.h}), \1, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (rshl_single_z1_z1_z0, svint16x2_t, svint16_t, z1,
		svrshl_single_s16_x2 (z1, z0),
		svrshl (z1, z0))

/*
** rshl_single_z18_z18_z0:
**	srshl	{z18\.h - z19\.h}, {z18\.h - z19\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (rshl_single_z18_z18_z0, svint16x2_t, svint16_t, z18,
		svrshl_single_s16_x2 (z18, z0),
		svrshl (z18, z0))

/*
** rshl_single_awkward:
**	...
**	srshl	({z[0-9]+\.h - z[0-9]+\.h}), \1, z[0-9]+\.h
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (rshl_single_awkward, svint16x2_t, svint16_t,
			z0_res = svrshl_single_s16_x2 (z1, z0),
			z0_res = svrshl (z1, z0))

/*
** rshl_single_z0_z0_z15:
**	...
**	srshl	{z0\.h - z1\.h}, {z0\.h - z1\.h}, z15\.h
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (rshl_single_z0_z0_z15, svint16x2_t, svint16_t,
		    z0 = svrshl_single_s16_x2 (z0, z15),
		    z0 = svrshl (z0, z15))

/*
** rshl_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	srshl	{z24\.h - z25\.h}, {z24\.h - z25\.h}, \1\.h
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z24_z16, svint16x2_t, svint16_t, z24,
		svrshl_single_s16_x2 (z24, z16),
		svrshl (z24, z16))
