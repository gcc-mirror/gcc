/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qdmulh_z0_z0_z4:
**	sqdmulh	{z0\.s - z3\.s}, {z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_XN (qdmulh_z0_z0_z4, svint32x4_t, z0,
	 svqdmulh_s32_x4 (z0, z4),
	 svqdmulh (z0, z4))

/*
** qdmulh_z0_z4_z0:
**	sqdmulh	{z0\.s - z3\.s}, {z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_XN (qdmulh_z0_z4_z0, svint32x4_t, z0,
	 svqdmulh_s32_x4 (z4, z0),
	 svqdmulh (z4, z0))

/*
** qdmulh_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	[^\n]+, {z28\.s - z31\.s}
** |
**	sqdmulh	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (qdmulh_z0_z4_z28, svint32x4_t, z0,
	 svqdmulh_s32_x4 (z4, z28),
	 svqdmulh (z4, z28))

/*
** qdmulh_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	[^\n]+, {z4\.s - z7\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (qdmulh_z18_z18_z4, svint32x4_t, z18,
	 svqdmulh_s32_x4 (z18, z4),
	 svqdmulh (z18, z4))

/*
** qdmulh_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (qdmulh_z23_z23_z28, svint32x4_t, z23,
	 svqdmulh_s32_x4 (z23, z28),
	 svqdmulh (z23, z28))

/*
** qdmulh_z28_z28_z0:
**	sqdmulh	{z28\.s - z31\.s}, {z28\.s - z31\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (qdmulh_z28_z28_z0, svint32x4_t, z28,
	 svqdmulh_s32_x4 (z28, z0),
	 svqdmulh (z28, z0))

/*
** qdmulh_z0_z0_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	{z0\.s - z3\.s}, {z0\.s - z3\.s}, [^\n]+
** |
**	sqdmulh	{z0\.s - z3\.s}, {z0\.s - z3\.s}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (qdmulh_z0_z0_z18, svint32x4_t, z0,
	 svqdmulh_s32_x4 (z0, z18),
	 svqdmulh (z0, z18))

/*
** qdmulh_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	{z4\.s - z7\.s}, {z4\.s - z7\.s}, [^\n]+
** |
**	sqdmulh	{z4\.s - z7\.s}, {z4\.s - z7\.s}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (qdmulh_z4_z4_z23, svint32x4_t, z4,
	 svqdmulh_s32_x4 (z4, z23),
	 svqdmulh (z4, z23))

/*
** qdmulh_single_z24_z24_z0:
**	sqdmulh	{z24\.s - z27\.s}, {z24\.s - z27\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z24_z24_z0, svint32x4_t, svint32_t, z24,
		svqdmulh_single_s32_x4 (z24, z0),
		svqdmulh (z24, z0))

/*
** qdmulh_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	{z24\.s - z27\.s}, {z24\.s - z27\.s}, z0\.s
** |
**	sqdmulh	{z28\.s - z31\.s}, {z28\.s - z31\.s}, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z24_z28_z0, svint32x4_t, svint32_t, z24,
		svqdmulh_single_s32_x4 (z28, z0),
		svqdmulh (z28, z0))

/*
** qdmulh_single_z24_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	{z24\.s - z27\.s}, {z24\.s - z27\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z24_z1_z0, svint32x4_t, svint32_t, z24,
		svqdmulh_single_s32_x4 (z1, z0),
		svqdmulh (z1, z0))

/*
** qdmulh_single_z1_z24_z0:
**	sqdmulh	{z24\.s - z27\.s}, {z24\.s - z27\.s}, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z1_z24_z0, svint32x4_t, svint32_t, z1,
		svqdmulh_single_s32_x4 (z24, z0),
		svqdmulh (z24, z0))

/*
** qdmulh_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	({z[0-9]+\.s - z[0-9]+\.s}), \1, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z1_z1_z0, svint32x4_t, svint32_t, z1,
		svqdmulh_single_s32_x4 (z1, z0),
		svqdmulh (z1, z0))

/*
** qdmulh_single_z18_z18_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqdmulh	[^\n]+, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z18_z18_z0, svint32x4_t, svint32_t, z18,
		svqdmulh_single_s32_x4 (z18, z0),
		svqdmulh (z18, z0))

/*
** qdmulh_single_awkward:
**	...
**	sqdmulh	({z[0-9]+\.s - z[0-9]+\.s}), \1, z[0-9]+\.s
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (qdmulh_single_awkward, svint32x4_t, svint32_t,
			z0_res = svqdmulh_single_s32_x4 (z1, z0),
			z0_res = svqdmulh (z1, z0))

/*
** qdmulh_single_z0_z0_z15:
**	...
**	sqdmulh	{z0\.s - z3\.s}, {z0\.s - z3\.s}, z15\.s
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (qdmulh_single_z0_z0_z15, svint32x4_t, svint32_t,
		    z0 = svqdmulh_single_s32_x4 (z0, z15),
		    z0 = svqdmulh (z0, z15))

/*
** qdmulh_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	sqdmulh	{z24\.s - z27\.s}, {z24\.s - z27\.s}, \1\.s
**	ret
*/
TEST_XN_SINGLE (qdmulh_single_z24_z24_z16, svint32x4_t, svint32_t, z24,
		svqdmulh_single_s32_x4 (z24, z16),
		svqdmulh (z24, z16))
