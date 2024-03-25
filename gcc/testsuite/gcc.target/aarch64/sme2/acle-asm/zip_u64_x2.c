/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** zip_z0_z0:
**	zip	{z0\.d - z1\.d}, z0\.d, z1\.d
**	ret
*/
TEST_XN (zip_z0_z0, svuint64x2_t, z0,
	 svzip_u64_x2 (z0),
	 svzip (z0))

/*
** zip_z0_z4:
**	zip	{z0\.d - z1\.d}, z4\.d, z5\.d
**	ret
*/
TEST_XN (zip_z0_z4, svuint64x2_t, z0,
	 svzip_u64_x2 (z4),
	 svzip (z4))

/*
** zip_z4_z18:
**	zip	{z4\.d - z5\.d}, z18\.d, z19\.d
**	ret
*/
TEST_XN (zip_z4_z18, svuint64x2_t, z4,
	 svzip_u64_x2 (z18),
	 svzip (z18))

/*
** zip_z18_z23:
**	zip	{z18\.d - z19\.d}, z23\.d, z24\.d
**	ret
*/
TEST_XN (zip_z18_z23, svuint64x2_t, z18,
	 svzip_u64_x2 (z23),
	 svzip (z23))

/*
** zip_z23_z28:
**	zip	[^\n]+, z28\.d, z29\.d
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (zip_z23_z28, svuint64x2_t, z23,
	 svzip_u64_x2 (z28),
	 svzip (z28))

/*
** zip_z28_z0:
**	zip	{z28\.d - z29\.d}, z0\.d, z1\.d
**	ret
*/
TEST_XN (zip_z28_z0, svuint64x2_t, z28,
	 svzip_u64_x2 (z0),
	 svzip (z0))

/*
** zip_z28_z0_z23:	{ xfail aarch64_big_endian }
**	zip	{z28\.d - z29\.d}, z0\.d, z23\.d
**	ret
*/
TEST_XN (zip_z28_z0_z23, svuint64x2_t, z28,
	 svzip_u64_x2 (svcreate2 (svget2 (z0, 0), svget2 (z23, 0))),
	 svzip (svcreate2 (svget2 (z0, 0), svget2 (z23, 0))))

/*
** zip_z28_z5_z19:
**	zip	{z28\.d - z29\.d}, z5\.d, z19\.d
**	ret
*/
TEST_XN (zip_z28_z5_z19, svuint64x2_t, z28,
	 svzip_u64_x2 (svcreate2 (svget2 (z4, 1), svget2 (z18, 1))),
	 svzip (svcreate2 (svget2 (z4, 1), svget2 (z18, 1))))
