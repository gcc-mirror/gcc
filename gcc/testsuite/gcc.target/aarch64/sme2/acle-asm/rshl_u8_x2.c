/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** rshl_z0_z0_z4:
**	urshl	{z0\.b - z1\.b}, {z0\.b - z1\.b}, {z4\.b - z5\.b}
**	ret
*/
TEST_DUAL_XN (rshl_z0_z0_z4, svuint8x2_t, svint8x2_t, z0,
	      svrshl_u8_x2 (z0, z4),
	      svrshl (z0, z4))

/*
** rshl_z4_z4_z0:
**	urshl	{z4\.b - z5\.b}, {z4\.b - z5\.b}, {z0\.b - z1\.b}
**	ret
*/
TEST_DUAL_XN (rshl_z4_z4_z0, svint8x2_t, svuint8x2_t, z4,
	      svrshl_u8_x2 (z4, z0),
	      svrshl (z4, z0))

/*
** rshl_z0_z28_z4:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	[^\n]+, {z4\.b - z5\.b}
** |
**	urshl	[^\n]+, {z4\.b - z5\.b}
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_DUAL_XN (rshl_z0_z28_z4, svuint8x2_t, svint8x2_t, z0,
	      svrshl_u8_x2 (z28, z4),
	      svrshl (z28, z4))

/*
** rshl_z18_z18_z4:
**	urshl	{z18\.b - z19\.b}, {z18\.b - z19\.b}, {z4\.b - z5\.b}
**	ret
*/
TEST_DUAL_XN (rshl_z18_z18_z4, svuint8x2_t, svint8x2_t, z18,
	      svrshl_u8_x2 (z18, z4),
	      svrshl (z18, z4))

/*
** rshl_z23_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	[^\n]+, {z18\.b - z19\.b}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (rshl_z23_z23_z18, svint8x2_t, svuint8x2_t, z23,
	      svrshl_u8_x2 (z23, z18),
	      svrshl (z23, z18))

/*
** rshl_z28_z28_z4:
**	urshl	{z28\.b - z29\.b}, {z28\.b - z29\.b}, {z4\.b - z5\.b}
**	ret
*/
TEST_DUAL_XN (rshl_z28_z28_z4, svuint8x2_t, svint8x2_t, z28,
	      svrshl_u8_x2 (z28, z4),
	      svrshl (z28, z4))

/*
** rshl_z4_z4_z18:
**	urshl	{z4\.b - z5\.b}, {z4\.b - z5\.b}, {z18\.b - z19\.b}
**	ret
*/
TEST_DUAL_XN (rshl_z4_z4_z18, svint8x2_t, svuint8x2_t, z4,
	      svrshl_u8_x2 (z4, z18),
	      svrshl (z4, z18))

/*
** rshl_z28_z28_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	{z28\.b - z29\.b}, {z28\.b - z29\.b}, [^\n]+
** |
**	urshl	{z28\.b - z29\.b}, {z28\.b - z29\.b}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_DUAL_XN (rshl_z28_z28_z23, svuint8x2_t, svint8x2_t, z28,
	      svrshl_u8_x2 (z28, z23),
	      svrshl (z28, z23))

/*
** rshl_single_z24_z24_z0:
**	urshl	{z24\.b - z25\.b}, {z24\.b - z25\.b}, z0\.b
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z24_z0, svuint8x2_t, svint8_t, z24,
		svrshl_single_u8_x2 (z24, z0),
		svrshl (z24, z0))

/*
** rshl_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	{z24\.b - z25\.b}, {z24\.b - z25\.b}, z0\.b
** |
**	urshl	{z28\.b - z29\.b}, {z28\.b - z29\.b}, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z28_z0, svuint8x2_t, svint8_t, z24,
		svrshl_single_u8_x2 (z28, z0),
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
**	urshl	{z24\.b - z25\.b}, {z24\.b - z25\.b}, z0\.b
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z1_z0, svuint8x2_t, svint8_t, z24,
		svrshl_single_u8_x2 (z1, z0),
		svrshl (z1, z0))

/*
** rshl_single_z1_z24_z0:
**	urshl	{z24\.b - z25\.b}, {z24\.b - z25\.b}, z0\.b
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (rshl_single_z1_z24_z0, svuint8x2_t, svint8_t, z1,
		svrshl_single_u8_x2 (z24, z0),
		svrshl (z24, z0))

/*
** rshl_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	({z[0-9]+\.b - z[0-9]+\.b}), \1, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (rshl_single_z1_z1_z0, svuint8x2_t, svint8_t, z1,
		svrshl_single_u8_x2 (z1, z0),
		svrshl (z1, z0))

/*
** rshl_single_z18_z18_z0:
**	urshl	{z18\.b - z19\.b}, {z18\.b - z19\.b}, z0\.b
**	ret
*/
TEST_XN_SINGLE (rshl_single_z18_z18_z0, svuint8x2_t, svint8_t, z18,
		svrshl_single_u8_x2 (z18, z0),
		svrshl (z18, z0))

/*
** rshl_single_awkward:
**	...
**	urshl	({z[0-9]+\.b - z[0-9]+\.b}), \1, z[0-9]+\.b
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (rshl_single_awkward, svuint8x2_t, svint8_t,
			z0_res = svrshl_single_u8_x2 (z1, z0),
			z0_res = svrshl (z1, z0))

/*
** rshl_single_z0_z0_z15:
**	...
**	urshl	{z0\.b - z1\.b}, {z0\.b - z1\.b}, z15\.b
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (rshl_single_z0_z0_z15, svuint8x2_t, svint8_t,
		    z0 = svrshl_single_u8_x2 (z0, z15),
		    z0 = svrshl (z0, z15))

/*
** rshl_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	urshl	{z24\.b - z25\.b}, {z24\.b - z25\.b}, \1\.b
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z24_z16, svuint8x2_t, svint8_t, z24,
		svrshl_single_u8_x2 (z24, z16),
		svrshl (z24, z16))
