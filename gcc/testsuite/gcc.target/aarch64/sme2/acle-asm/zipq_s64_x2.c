/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** zipq_z0_z0:
**	zip	{z0\.q - z1\.q}, z0\.q, z1\.q
**	ret
*/
TEST_XN (zipq_z0_z0, svint64x2_t, z0,
	 svzipq_s64_x2 (z0),
	 svzipq (z0))

/*
** zipq_z0_z4:
**	zip	{z0\.q - z1\.q}, z4\.q, z5\.q
**	ret
*/
TEST_XN (zipq_z0_z4, svint64x2_t, z0,
	 svzipq_s64_x2 (z4),
	 svzipq (z4))

/*
** zipq_z4_z18:
**	zip	{z4\.q - z5\.q}, z18\.q, z19\.q
**	ret
*/
TEST_XN (zipq_z4_z18, svint64x2_t, z4,
	 svzipq_s64_x2 (z18),
	 svzipq (z18))

/*
** zipq_z18_z23:
**	zip	{z18\.q - z19\.q}, z23\.q, z24\.q
**	ret
*/
TEST_XN (zipq_z18_z23, svint64x2_t, z18,
	 svzipq_s64_x2 (z23),
	 svzipq (z23))

/*
** zipq_z23_z28:
**	zip	[^\n]+, z28\.q, z29\.q
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (zipq_z23_z28, svint64x2_t, z23,
	 svzipq_s64_x2 (z28),
	 svzipq (z28))

/*
** zipq_z28_z0:
**	zip	{z28\.q - z29\.q}, z0\.q, z1\.q
**	ret
*/
TEST_XN (zipq_z28_z0, svint64x2_t, z28,
	 svzipq_s64_x2 (z0),
	 svzipq (z0))

/*
** zipq_z28_z0_z23:	{ xfail aarch64_big_endian }
**	zip	{z28\.q - z29\.q}, z0\.q, z23\.q
**	ret
*/
TEST_XN (zipq_z28_z0_z23, svint64x2_t, z28,
	 svzipq_s64_x2 (svcreate2 (svget2 (z0, 0), svget2 (z23, 0))),
	 svzipq (svcreate2 (svget2 (z0, 0), svget2 (z23, 0))))

/*
** zipq_z28_z5_z19:
**	zip	{z28\.q - z29\.q}, z5\.q, z19\.q
**	ret
*/
TEST_XN (zipq_z28_z5_z19, svint64x2_t, z28,
	 svzipq_s64_x2 (svcreate2 (svget2 (z4, 1), svget2 (z18, 1))),
	 svzipq (svcreate2 (svget2 (z4, 1), svget2 (z18, 1))))
