/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** add_z24_z24_z0:
**	add	{z24\.d - z25\.d}, {z24\.d - z25\.d}, z0\.d
**	ret
*/
TEST_XN_SINGLE (add_z24_z24_z0, svuint64x2_t, svuint64_t, z24,
		svadd_single_u64_x2 (z24, z0),
		svadd (z24, z0))

/*
** add_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	add	{z24\.d - z25\.d}, {z24\.d - z25\.d}, z0\.d
** |
**	add	{z28\.d - z29\.d}, {z28\.d - z29\.d}, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (add_z24_z28_z0, svuint64x2_t, svuint64_t, z24,
		svadd_single_u64_x2 (z28, z0),
		svadd (z28, z0))

/*
** add_z24_z1_z0:
** (
**	mov	z24\.d, z1\.d
**	mov	z25\.d, z2\.d
** |
**	mov	z25\.d, z2\.d
**	mov	z24\.d, z1\.d
** )
**	add	{z24\.d - z25\.d}, {z24\.d - z25\.d}, z0\.d
**	ret
*/
TEST_XN_SINGLE (add_z24_z1_z0, svuint64x2_t, svuint64_t, z24,
		svadd_single_u64_x2 (z1, z0),
		svadd (z1, z0))

/*
** add_z1_z24_z0:
**	add	{z24\.d - z25\.d}, {z24\.d - z25\.d}, z0\.d
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (add_z1_z24_z0, svuint64x2_t, svuint64_t, z1,
		svadd_single_u64_x2 (z24, z0),
		svadd (z24, z0))

/*
** add_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	add	({z[0-9]+\.d - z[0-9]+\.d}), \1, z0\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (add_z1_z1_z0, svuint64x2_t, svuint64_t, z1,
		svadd_single_u64_x2 (z1, z0),
		svadd (z1, z0))

/*
** add_z18_z18_z0:
**	add	{z18\.d - z19\.d}, {z18\.d - z19\.d}, z0\.d
**	ret
*/
TEST_XN_SINGLE (add_z18_z18_z0, svuint64x2_t, svuint64_t, z18,
		svadd_single_u64_x2 (z18, z0),
		svadd (z18, z0))

/*
** add_awkward:
**	...
**	add	({z[0-9]+\.d - z[0-9]+\.d}), \1, z[0-9]+\.d
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (add_awkward, svuint64x2_t, svuint64_t,
			z0_res = svadd_single_u64_x2 (z1, z0),
			z0_res = svadd (z1, z0))

/*
** add_z0_z0_z15:
**	...
**	add	{z0\.d - z1\.d}, {z0\.d - z1\.d}, z15\.d
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (add_z0_z0_z15, svuint64x2_t, svuint64_t,
		    z0 = svadd_single_u64_x2 (z0, z15),
		    z0 = svadd (z0, z15))

/*
** add_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	add	{z24\.d - z25\.d}, {z24\.d - z25\.d}, \1\.d
**	ret
*/
TEST_XN_SINGLE (add_z24_z24_z16, svuint64x2_t, svuint64_t, z24,
		svadd_single_u64_x2 (z24, z16),
		svadd (z24, z16))
