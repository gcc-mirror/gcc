/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** add_z24_z24_z0:
**	add	{z24\.s - z27\.s}, {z24\.s - z27\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (add_z24_z24_z0, svint32x4_t, svint32_t, z24,
		svadd_single_s32_x4 (z24, z0),
		svadd (z24, z0))

/*
** add_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	add	{z24\.s - z27\.s}, {z24\.s - z27\.s}, z0\.s
** |
**	add	{z28\.s - z31\.s}, {z28\.s - z31\.s}, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (add_z24_z28_z0, svint32x4_t, svint32_t, z24,
		svadd_single_s32_x4 (z28, z0),
		svadd (z28, z0))

/*
** add_z24_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	add	{z24\.s - z27\.s}, {z24\.s - z27\.s}, z0\.s
**	ret
*/
TEST_XN_SINGLE (add_z24_z1_z0, svint32x4_t, svint32_t, z24,
		svadd_single_s32_x4 (z1, z0),
		svadd (z1, z0))

/*
** add_z1_z24_z0:
**	add	{z24\.s - z27\.s}, {z24\.s - z27\.s}, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (add_z1_z24_z0, svint32x4_t, svint32_t, z1,
		svadd_single_s32_x4 (z24, z0),
		svadd (z24, z0))

/*
** add_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	add	({z[0-9]+\.s - z[0-9]+\.s}), \1, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (add_z1_z1_z0, svint32x4_t, svint32_t, z1,
		svadd_single_s32_x4 (z1, z0),
		svadd (z1, z0))

/*
** add_z18_z18_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	add	({z[0-9]+\.s - z[0-9]+\.s}), \1, z0\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (add_z18_z18_z0, svint32x4_t, svint32_t, z18,
		svadd_single_s32_x4 (z18, z0),
		svadd (z18, z0))

/*
** add_awkward:
**	...
**	add	({z[0-9]+\.s - z[0-9]+\.s}), \1, z[0-9]+\.s
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (add_awkward, svint32x4_t, svint32_t,
			z0_res = svadd_single_s32_x4 (z1, z0),
			z0_res = svadd (z1, z0))

/*
** add_z0_z0_z15:
**	...
**	add	{z0\.s - z3\.s}, {z0\.s - z3\.s}, z15\.s
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (add_z0_z0_z15, svint32x4_t, svint32_t,
		    z0 = svadd_single_s32_x4 (z0, z15),
		    z0 = svadd (z0, z15))

/*
** add_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	add	{z24\.s - z27\.s}, {z24\.s - z27\.s}, \1\.s
**	ret
*/
TEST_XN_SINGLE (add_z24_z24_z16, svint32x4_t, svint32_t, z24,
		svadd_single_s32_x4 (z24, z16),
		svadd (z24, z16))
