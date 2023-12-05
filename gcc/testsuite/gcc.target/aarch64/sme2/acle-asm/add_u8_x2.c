/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** add_z24_z24_z0:
**	add	{z24\.b - z25\.b}, {z24\.b - z25\.b}, z0\.b
**	ret
*/
TEST_XN_SINGLE (add_z24_z24_z0, svuint8x2_t, svuint8_t, z24,
		svadd_single_u8_x2 (z24, z0),
		svadd (z24, z0))

/*
** add_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	add	{z24\.b - z25\.b}, {z24\.b - z25\.b}, z0\.b
** |
**	add	{z28\.b - z29\.b}, {z28\.b - z29\.b}, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (add_z24_z28_z0, svuint8x2_t, svuint8_t, z24,
		svadd_single_u8_x2 (z28, z0),
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
**	add	{z24\.b - z25\.b}, {z24\.b - z25\.b}, z0\.b
**	ret
*/
TEST_XN_SINGLE (add_z24_z1_z0, svuint8x2_t, svuint8_t, z24,
		svadd_single_u8_x2 (z1, z0),
		svadd (z1, z0))

/*
** add_z1_z24_z0:
**	add	{z24\.b - z25\.b}, {z24\.b - z25\.b}, z0\.b
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (add_z1_z24_z0, svuint8x2_t, svuint8_t, z1,
		svadd_single_u8_x2 (z24, z0),
		svadd (z24, z0))

/*
** add_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	add	({z[0-9]+\.b - z[0-9]+\.b}), \1, z0\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (add_z1_z1_z0, svuint8x2_t, svuint8_t, z1,
		svadd_single_u8_x2 (z1, z0),
		svadd (z1, z0))

/*
** add_z18_z18_z0:
**	add	{z18\.b - z19\.b}, {z18\.b - z19\.b}, z0\.b
**	ret
*/
TEST_XN_SINGLE (add_z18_z18_z0, svuint8x2_t, svuint8_t, z18,
		svadd_single_u8_x2 (z18, z0),
		svadd (z18, z0))

/*
** add_awkward:
**	...
**	add	({z[0-9]+\.b - z[0-9]+\.b}), \1, z[0-9]+\.b
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (add_awkward, svuint8x2_t, svuint8_t,
			z0_res = svadd_single_u8_x2 (z1, z0),
			z0_res = svadd (z1, z0))

/*
** add_z0_z0_z15:
**	...
**	add	{z0\.b - z1\.b}, {z0\.b - z1\.b}, z15\.b
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (add_z0_z0_z15, svuint8x2_t, svuint8_t,
		    z0 = svadd_single_u8_x2 (z0, z15),
		    z0 = svadd (z0, z15))

/*
** add_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	add	{z24\.b - z25\.b}, {z24\.b - z25\.b}, \1\.b
**	ret
*/
TEST_XN_SINGLE (add_z24_z24_z16, svuint8x2_t, svuint8_t, z24,
		svadd_single_u8_x2 (z24, z16),
		svadd (z24, z16))
