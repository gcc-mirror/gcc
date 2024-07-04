/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** sel_z0_pn0_z0_z4:
**	mov	p([0-9]+)\.b, p0\.b
**	sel	{z0\.d - z1\.d}, pn\1, {z0\.d - z1\.d}, {z4\.d - z5\.d}
**	ret
*/
TEST_XN (sel_z0_pn0_z0_z4, svint64x2_t, z0,
	 svsel_s64_x2 (pn0, z0, z4),
	 svsel (pn0, z0, z4))

/*
** sel_z0_pn7_z0_z4:
**	mov	p([0-9]+)\.b, p7\.b
**	sel	{z0\.d - z1\.d}, pn\1, {z0\.d - z1\.d}, {z4\.d - z5\.d}
**	ret
*/
TEST_XN (sel_z0_pn7_z0_z4, svint64x2_t, z0,
	 svsel_s64_x2 (pn7, z0, z4),
	 svsel (pn7, z0, z4))

/*
** sel_z0_pn8_z4_z28:
**	sel	{z0\.d - z1\.d}, pn8, {z4\.d - z5\.d}, {z28\.d - z29\.d}
**	ret
*/
TEST_XN (sel_z0_pn8_z4_z28, svint64x2_t, z0,
	 svsel_s64_x2 (pn8, z4, z28),
	 svsel (pn8, z4, z28))

/*
** sel_z4_pn8_z18_z0:
**	sel	{z4\.d - z5\.d}, pn8, {z18\.d - z19\.d}, {z0\.d - z1\.d}
**	ret
*/
TEST_XN (sel_z4_pn8_z18_z0, svint64x2_t, z4,
	 svsel_s64_x2 (pn8, z18, z0),
	 svsel (pn8, z18, z0))

/*
** sel_z18_pn15_z28_z4:
**	sel	{z18\.d - z19\.d}, pn15, {z28\.d - z29\.d}, {z4\.d - z5\.d}
**	ret
*/
TEST_XN (sel_z18_pn15_z28_z4, svint64x2_t, z18,
	 svsel_s64_x2 (pn15, z28, z4),
	 svsel (pn15, z28, z4))

/*
** sel_z18_pn8_z18_z4:
**	sel	{z18\.d - z19\.d}, pn8, {z18\.d - z19\.d}, {z4\.d - z5\.d}
**	ret
*/
TEST_XN (sel_z18_pn8_z18_z4, svint64x2_t, z18,
	 svsel_s64_x2 (pn8, z18, z4),
	 svsel (pn8, z18, z4))

/*
** sel_z23_pn15_z0_z18:
**	sel	[^\n]+, pn15, {z0\.d - z1\.d}, {z18\.d - z19\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (sel_z23_pn15_z0_z18, svint64x2_t, z23,
	 svsel_s64_x2 (pn15, z0, z18),
	 svsel (pn15, z0, z18))

/*
** sel_z0_pn15_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	sel	{z0\.d - z1\.d}, pn15, {[^}]+}, {z28\.d - z29\.d}
**	ret
*/
TEST_XN (sel_z0_pn15_z23_z28, svint64x2_t, z0,
	 svsel_s64_x2 (pn15, z23, z28),
	 svsel (pn15, z23, z28))

/*
** sel_z0_pn8_z28_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	sel	{z0\.d - z1\.d}, pn8, {z28\.d - z29\.d}, {[^}]+}
**	ret
*/
TEST_XN (sel_z0_pn8_z28_z23, svint64x2_t, z0,
	 svsel_s64_x2 (pn8, z28, z23),
	 svsel (pn8, z28, z23))
