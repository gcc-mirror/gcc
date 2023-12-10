/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** zip_z0_z0:
**	zip	{z0\.h - z1\.h}, z0\.h, z1\.h
**	ret
*/
TEST_XN (zip_z0_z0, svbfloat16x2_t, z0,
	 svzip_bf16_x2 (z0),
	 svzip (z0))

/*
** zip_z0_z4:
**	zip	{z0\.h - z1\.h}, z4\.h, z5\.h
**	ret
*/
TEST_XN (zip_z0_z4, svbfloat16x2_t, z0,
	 svzip_bf16_x2 (z4),
	 svzip (z4))

/*
** zip_z4_z18:
**	zip	{z4\.h - z5\.h}, z18\.h, z19\.h
**	ret
*/
TEST_XN (zip_z4_z18, svbfloat16x2_t, z4,
	 svzip_bf16_x2 (z18),
	 svzip (z18))

/*
** zip_z18_z23:
**	zip	{z18\.h - z19\.h}, z23\.h, z24\.h
**	ret
*/
TEST_XN (zip_z18_z23, svbfloat16x2_t, z18,
	 svzip_bf16_x2 (z23),
	 svzip (z23))

/*
** zip_z23_z28:
**	zip	[^\n]+, z28\.h, z29\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (zip_z23_z28, svbfloat16x2_t, z23,
	 svzip_bf16_x2 (z28),
	 svzip (z28))

/*
** zip_z28_z0:
**	zip	{z28\.h - z29\.h}, z0\.h, z1\.h
**	ret
*/
TEST_XN (zip_z28_z0, svbfloat16x2_t, z28,
	 svzip_bf16_x2 (z0),
	 svzip (z0))

/*
** zip_z28_z0_z23:	{ xfail aarch64_big_endian }
**	zip	{z28\.h - z29\.h}, z0\.h, z23\.h
**	ret
*/
TEST_XN (zip_z28_z0_z23, svbfloat16x2_t, z28,
	 svzip_bf16_x2 (svcreate2 (svget2 (z0, 0), svget2 (z23, 0))),
	 svzip (svcreate2 (svget2 (z0, 0), svget2 (z23, 0))))

/*
** zip_z28_z5_z19:
**	zip	{z28\.h - z29\.h}, z5\.h, z19\.h
**	ret
*/
TEST_XN (zip_z28_z5_z19, svbfloat16x2_t, z28,
	 svzip_bf16_x2 (svcreate2 (svget2 (z4, 1), svget2 (z18, 1))),
	 svzip (svcreate2 (svget2 (z4, 1), svget2 (z18, 1))))
