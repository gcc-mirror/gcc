/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qdmulh_z0_z0_z4:
**	sqdmulh	{z0\.b - z3\.b}, {z0\.b - z3\.b}, {z4\.b - z7\.b}
**	ret
*/
TEST_XN (qdmulh_z0_z0_z4, svint8x4_t, z0,
	 svqdmulh_s8_x4 (z0, z4),
	 svqdmulh (z0, z4))

/*
** qdmulh_z0_z4_z0:
**	sqdmulh	{z0\.b - z3\.b}, {z0\.b - z3\.b}, {z4\.b - z7\.b}
**	ret
*/
TEST_XN (qdmulh_z0_z4_z0, svint8x4_t, z0,
	 svqdmulh_s8_x4 (z4, z0),
	 svqdmulh (z4, z0))

/*
** qdmulh_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	[^\n]+, {z28\.b - z31\.b}
** |
**	sqdmulh	[^\n]+, {z28\.b - z31\.b}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (qdmulh_z0_z4_z28, svint8x4_t, z0,
	 svqdmulh_s8_x4 (z4, z28),
	 svqdmulh (z4, z28))

/*
** qdmulh_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	[^\n]+, {z4\.b - z7\.b}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (qdmulh_z18_z18_z4, svint8x4_t, z18,
	 svqdmulh_s8_x4 (z18, z4),
	 svqdmulh (z18, z4))

/*
** qdmulh_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	[^\n]+, {z28\.b - z31\.b}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (qdmulh_z23_z23_z28, svint8x4_t, z23,
	 svqdmulh_s8_x4 (z23, z28),
	 svqdmulh (z23, z28))

/*
** qdmulh_z28_z28_z0:
**	sqdmulh	{z28\.b - z31\.b}, {z28\.b - z31\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_XN (qdmulh_z28_z28_z0, svint8x4_t, z28,
	 svqdmulh_s8_x4 (z28, z0),
	 svqdmulh (z28, z0))

/*
** qdmulh_z0_z0_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	{z0\.b - z3\.b}, {z0\.b - z3\.b}, [^\n]+
** |
**	sqdmulh	{z0\.b - z3\.b}, {z0\.b - z3\.b}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (qdmulh_z0_z0_z18, svint8x4_t, z0,
	 svqdmulh_s8_x4 (z0, z18),
	 svqdmulh (z0, z18))

/*
** qdmulh_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	{z4\.b - z7\.b}, {z4\.b - z7\.b}, [^\n]+
** |
**	sqdmulh	{z4\.b - z7\.b}, {z4\.b - z7\.b}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (qdmulh_z4_z4_z23, svint8x4_t, z4,
	 svqdmulh_s8_x4 (z4, z23),
	 svqdmulh (z4, z23))

/*
** qdmulh_single_z24_z24_z0:
**	sqdmulh	{z24\.b - z27\.b}, {z24\.b - z27\.b}, z0\.b
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z24_z24_z0, svint8x4_t, svint8_t, z24,
		svqdmulh_single_s8_x4 (z24, z0),
		svqdmulh (z24, z0))

/*
** qdmulh_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	{z24\.b - z27\.b}, {z24\.b - z27\.b}, z0\.b
** |
**	sqdmulh	{z28\.b - z31\.b}, {z28\.b - z31\.b}, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z24_z28_z0, svint8x4_t, svint8_t, z24,
		svqdmulh_single_s8_x4 (z28, z0),
		svqdmulh (z28, z0))

/*
** qdmulh_single_z24_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	{z24\.b - z27\.b}, {z24\.b - z27\.b}, z0\.b
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z24_z1_z0, svint8x4_t, svint8_t, z24,
		svqdmulh_single_s8_x4 (z1, z0),
		svqdmulh (z1, z0))

/*
** qdmulh_single_z1_z24_z0:
**	sqdmulh	{z24\.b - z27\.b}, {z24\.b - z27\.b}, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z1_z24_z0, svint8x4_t, svint8_t, z1,
		svqdmulh_single_s8_x4 (z24, z0),
		svqdmulh (z24, z0))

/*
** qdmulh_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	({z[0-9]+\.b - z[0-9]+\.b}), \1, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z1_z1_z0, svint8x4_t, svint8_t, z1,
		svqdmulh_single_s8_x4 (z1, z0),
		svqdmulh (z1, z0))

/*
** qdmulh_single_z18_z18_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	[^\n]+, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z18_z18_z0, svint8x4_t, svint8_t, z18,
		svqdmulh_single_s8_x4 (z18, z0),
		svqdmulh (z18, z0))

/*
** qdmulh_single_awkward:
**	...
**	sqdmulh	({z[0-9]+\.b - z[0-9]+\.b}), \1, z[0-9]+\.b
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (qdmulh_single_awkward, svint8x4_t, svint8_t,
			z0_res = svqdmulh_single_s8_x4 (z1, z0),
			z0_res = svqdmulh (z1, z0))

/*
** qdmulh_single_z0_z0_z15:
**	...
**	sqdmulh	{z0\.b - z3\.b}, {z0\.b - z3\.b}, z15\.b
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (qdmulh_single_z0_z0_z15, svint8x4_t, svint8_t,
		    z0 = svqdmulh_single_s8_x4 (z0, z15),
		    z0 = svqdmulh (z0, z15))

/*
** qdmulh_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	sqdmulh	{z24\.b - z27\.b}, {z24\.b - z27\.b}, \1\.b
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z24_z24_z16, svint8x4_t, svint8_t, z24,
		svqdmulh_single_s8_x4 (z24, z16),
		svqdmulh (z24, z16))
