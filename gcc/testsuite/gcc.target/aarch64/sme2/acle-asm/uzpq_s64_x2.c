/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** uzpq_z0_z0:
**	uzp	{z0\.q - z1\.q}, z0\.q, z1\.q
**	ret
*/
TEST_XN (uzpq_z0_z0, svint64x2_t, z0,
	 svuzpq_s64_x2 (z0),
	 svuzpq (z0))

/*
** uzpq_z0_z4:
**	uzp	{z0\.q - z1\.q}, z4\.q, z5\.q
**	ret
*/
TEST_XN (uzpq_z0_z4, svint64x2_t, z0,
	 svuzpq_s64_x2 (z4),
	 svuzpq (z4))

/*
** uzpq_z4_z18:
**	uzp	{z4\.q - z5\.q}, z18\.q, z19\.q
**	ret
*/
TEST_XN (uzpq_z4_z18, svint64x2_t, z4,
	 svuzpq_s64_x2 (z18),
	 svuzpq (z18))

/*
** uzpq_z18_z23:
**	uzp	{z18\.q - z19\.q}, z23\.q, z24\.q
**	ret
*/
TEST_XN (uzpq_z18_z23, svint64x2_t, z18,
	 svuzpq_s64_x2 (z23),
	 svuzpq (z23))

/*
** uzpq_z23_z28:
**	uzp	[^\n]+, z28\.q, z29\.q
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (uzpq_z23_z28, svint64x2_t, z23,
	 svuzpq_s64_x2 (z28),
	 svuzpq (z28))

/*
** uzpq_z28_z0:
**	uzp	{z28\.q - z29\.q}, z0\.q, z1\.q
**	ret
*/
TEST_XN (uzpq_z28_z0, svint64x2_t, z28,
	 svuzpq_s64_x2 (z0),
	 svuzpq (z0))

/*
** uzpq_z28_z0_z23:	{ xfail aarch64_big_endian }
**	uzp	{z28\.q - z29\.q}, z0\.q, z23\.q
**	ret
*/
TEST_XN (uzpq_z28_z0_z23, svint64x2_t, z28,
	 svuzpq_s64_x2 (svcreate2 (svget2 (z0, 0), svget2 (z23, 0))),
	 svuzpq (svcreate2 (svget2 (z0, 0), svget2 (z23, 0))))

/*
** uzpq_z28_z5_z19:
**	uzp	{z28\.q - z29\.q}, z5\.q, z19\.q
**	ret
*/
TEST_XN (uzpq_z28_z5_z19, svint64x2_t, z28,
	 svuzpq_s64_x2 (svcreate2 (svget2 (z4, 1), svget2 (z18, 1))),
	 svuzpq (svcreate2 (svget2 (z4, 1), svget2 (z18, 1))))
