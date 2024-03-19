/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** rshl_z0_z0_z4:
**	urshl	{z0\.h - z3\.h}, {z0\.h - z3\.h}, {z4\.h - z7\.h}
**	ret
*/
TEST_DUAL_XN (rshl_z0_z0_z4, svuint16x4_t, svint16x4_t, z0,
	      svrshl_u16_x4 (z0, z4),
	      svrshl (z0, z4))

/*
** rshl_z4_z4_z0:
**	urshl	{z4\.h - z7\.h}, {z4\.h - z7\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_DUAL_XN (rshl_z4_z4_z0, svint16x4_t, svuint16x4_t, z4,
	      svrshl_u16_x4 (z4, z0),
	      svrshl (z4, z0))

/*
** rshl_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	[^\n]+, {z4\.h - z7\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (rshl_z18_z18_z4, svuint16x4_t, svint16x4_t, z18,
	      svrshl_u16_x4 (z18, z4),
	      svrshl (z18, z4))

/*
** rshl_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	[^\n]+, {z28\.h - z31\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (rshl_z23_z23_z28, svint16x4_t, svuint16x4_t, z23,
	      svrshl_u16_x4 (z23, z28),
	      svrshl (z23, z28))

/*
** rshl_z28_z28_z4:
**	urshl	{z28\.h - z31\.h}, {z28\.h - z31\.h}, {z4\.h - z7\.h}
**	ret
*/
TEST_DUAL_XN (rshl_z28_z28_z4, svuint16x4_t, svint16x4_t, z28,
	      svrshl_u16_x4 (z28, z4),
	      svrshl (z28, z4))

/*
** rshl_z4_z4_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	{z4\.h - z7\.h}, {z4\.h - z7\.h}, [^\n]+
** |
**	urshl	{z4\.h - z7\.h}, {z4\.h - z7\.h}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_DUAL_XN (rshl_z4_z4_z18, svint16x4_t, svuint16x4_t, z4,
	      svrshl_u16_x4 (z4, z18),
	      svrshl (z4, z18))

/*
** rshl_z0_z0_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	{z0\.h - z3\.h}, {z0\.h - z3\.h}, [^\n]+
** |
**	urshl	{z0\.h - z3\.h}, {z0\.h - z3\.h}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_DUAL_XN (rshl_z0_z0_z23, svuint16x4_t, svint16x4_t, z0,
	      svrshl_u16_x4 (z0, z23),
	      svrshl (z0, z23))

/*
** rshl_single_z24_z24_z0:
**	urshl	{z24\.h - z27\.h}, {z24\.h - z27\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z24_z0, svuint16x4_t, svint16_t, z24,
		svrshl_single_u16_x4 (z24, z0),
		svrshl (z24, z0))

/*
** rshl_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	{z24\.h - z27\.h}, {z24\.h - z27\.h}, z0\.h
** |
**	urshl	{z28\.h - z31\.h}, {z28\.h - z31\.h}, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z28_z0, svuint16x4_t, svint16_t, z24,
		svrshl_single_u16_x4 (z28, z0),
		svrshl (z28, z0))

/*
** rshl_single_z24_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	{z24\.h - z27\.h}, {z24\.h - z27\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z1_z0, svuint16x4_t, svint16_t, z24,
		svrshl_single_u16_x4 (z1, z0),
		svrshl (z1, z0))

/*
** rshl_single_z1_z24_z0:
**	urshl	{z24\.h - z27\.h}, {z24\.h - z27\.h}, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (rshl_single_z1_z24_z0, svuint16x4_t, svint16_t, z1,
		svrshl_single_u16_x4 (z24, z0),
		svrshl (z24, z0))

/*
** rshl_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	({z[0-9]+\.h - z[0-9]+\.h}), \1, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (rshl_single_z1_z1_z0, svuint16x4_t, svint16_t, z1,
		svrshl_single_u16_x4 (z1, z0),
		svrshl (z1, z0))

/*
** rshl_single_z18_z18_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	urshl	[^\n]+, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (rshl_single_z18_z18_z0, svuint16x4_t, svint16_t, z18,
		svrshl_single_u16_x4 (z18, z0),
		svrshl (z18, z0))

/*
** rshl_single_awkward:
**	...
**	urshl	({z[0-9]+\.h - z[0-9]+\.h}), \1, z[0-9]+\.h
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (rshl_single_awkward, svuint16x4_t, svint16_t,
			z0_res = svrshl_single_u16_x4 (z1, z0),
			z0_res = svrshl (z1, z0))

/*
** rshl_single_z0_z0_z15:
**	...
**	urshl	{z0\.h - z3\.h}, {z0\.h - z3\.h}, z15\.h
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (rshl_single_z0_z0_z15, svuint16x4_t, svint16_t,
		    z0 = svrshl_single_u16_x4 (z0, z15),
		    z0 = svrshl (z0, z15))

/*
** rshl_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	urshl	{z24\.h - z27\.h}, {z24\.h - z27\.h}, \1\.h
**	ret
*/
TEST_XN_SINGLE (rshl_single_z24_z24_z16, svuint16x4_t, svint16_t, z24,
		svrshl_single_u16_x4 (z24, z16),
		svrshl (z24, z16))
