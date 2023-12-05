/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** min_z0_z0_z4:
**	umin	{z0\.b - z3\.b}, {z0\.b - z3\.b}, {z4\.b - z7\.b}
**	ret
*/
TEST_XN (min_z0_z0_z4, svuint8x4_t, z0,
	 svmin_u8_x4 (z0, z4),
	 svmin (z0, z4))

/*
** min_z0_z4_z0:
**	umin	{z0\.b - z3\.b}, {z0\.b - z3\.b}, {z4\.b - z7\.b}
**	ret
*/
TEST_XN (min_z0_z4_z0, svuint8x4_t, z0,
	 svmin_u8_x4 (z4, z0),
	 svmin (z4, z0))

/*
** min_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	umin	[^\n]+, {z28\.b - z31\.b}
** |
**	umin	[^\n]+, {z28\.b - z31\.b}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (min_z0_z4_z28, svuint8x4_t, z0,
	 svmin_u8_x4 (z4, z28),
	 svmin (z4, z28))

/*
** min_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	umin	[^\n]+, {z4\.b - z7\.b}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (min_z18_z18_z4, svuint8x4_t, z18,
	 svmin_u8_x4 (z18, z4),
	 svmin (z18, z4))

/*
** min_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	umin	[^\n]+, {z28\.b - z31\.b}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (min_z23_z23_z28, svuint8x4_t, z23,
	 svmin_u8_x4 (z23, z28),
	 svmin (z23, z28))

/*
** min_z28_z28_z0:
**	umin	{z28\.b - z31\.b}, {z28\.b - z31\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_XN (min_z28_z28_z0, svuint8x4_t, z28,
	 svmin_u8_x4 (z28, z0),
	 svmin (z28, z0))

/*
** min_z0_z0_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	umin	{z0\.b - z3\.b}, {z0\.b - z3\.b}, [^\n]+
** |
**	umin	{z0\.b - z3\.b}, {z0\.b - z3\.b}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (min_z0_z0_z18, svuint8x4_t, z0,
	 svmin_u8_x4 (z0, z18),
	 svmin (z0, z18))

/*
** min_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	umin	{z4\.b - z7\.b}, {z4\.b - z7\.b}, [^\n]+
** |
**	umin	{z4\.b - z7\.b}, {z4\.b - z7\.b}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (min_z4_z4_z23, svuint8x4_t, z4,
	 svmin_u8_x4 (z4, z23),
	 svmin (z4, z23))

/*
** min_single_z24_z24_z0:
**	umin	{z24\.b - z27\.b}, {z24\.b - z27\.b}, z0\.b
**	ret
*/
TEST_XN_SINGLE (min_single_z24_z24_z0, svuint8x4_t, svuint8_t, z24,
		svmin_single_u8_x4 (z24, z0),
		svmin (z24, z0))

/*
** min_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	umin	{z24\.b - z27\.b}, {z24\.b - z27\.b}, z0\.b
** |
**	umin	{z28\.b - z31\.b}, {z28\.b - z31\.b}, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (min_single_z24_z28_z0, svuint8x4_t, svuint8_t, z24,
		svmin_single_u8_x4 (z28, z0),
		svmin (z28, z0))

/*
** min_single_z24_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	umin	{z24\.b - z27\.b}, {z24\.b - z27\.b}, z0\.b
**	ret
*/
TEST_XN_SINGLE (min_single_z24_z1_z0, svuint8x4_t, svuint8_t, z24,
		svmin_single_u8_x4 (z1, z0),
		svmin (z1, z0))

/*
** min_single_z1_z24_z0:
**	umin	{z24\.b - z27\.b}, {z24\.b - z27\.b}, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (min_single_z1_z24_z0, svuint8x4_t, svuint8_t, z1,
		svmin_single_u8_x4 (z24, z0),
		svmin (z24, z0))

/*
** min_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	umin	({z[0-9]+\.b - z[0-9]+\.b}), \1, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (min_single_z1_z1_z0, svuint8x4_t, svuint8_t, z1,
		svmin_single_u8_x4 (z1, z0),
		svmin (z1, z0))

/*
** min_single_z18_z18_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	umin	[^\n]+, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (min_single_z18_z18_z0, svuint8x4_t, svuint8_t, z18,
		svmin_single_u8_x4 (z18, z0),
		svmin (z18, z0))

/*
** min_single_awkward:
**	...
**	umin	({z[0-9]+\.b - z[0-9]+\.b}), \1, z[0-9]+\.b
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (min_single_awkward, svuint8x4_t, svuint8_t,
			z0_res = svmin_single_u8_x4 (z1, z0),
			z0_res = svmin (z1, z0))

/*
** min_single_z0_z0_z15:
**	...
**	umin	{z0\.b - z3\.b}, {z0\.b - z3\.b}, z15\.b
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (min_single_z0_z0_z15, svuint8x4_t, svuint8_t,
		    z0 = svmin_single_u8_x4 (z0, z15),
		    z0 = svmin (z0, z15))

/*
** min_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	umin	{z24\.b - z27\.b}, {z24\.b - z27\.b}, \1\.b
**	ret
*/
TEST_XN_SINGLE (min_single_z24_z24_z16, svuint8x4_t, svuint8_t, z24,
		svmin_single_u8_x4 (z24, z16),
		svmin (z24, z16))
