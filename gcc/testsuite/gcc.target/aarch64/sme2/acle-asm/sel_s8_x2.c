/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** sel_z0_pn0_z0_z4:
**	mov	p([0-9]+)\.b, p0\.b
**	sel	{z0\.b - z1\.b}, pn\1, {z0\.b - z1\.b}, {z4\.b - z5\.b}
**	ret
*/
TEST_XN (sel_z0_pn0_z0_z4, svint8x2_t, z0,
	 svsel_s8_x2 (pn0, z0, z4),
	 svsel (pn0, z0, z4))

/*
** sel_z0_pn7_z0_z4:
**	mov	p([0-9]+)\.b, p7\.b
**	sel	{z0\.b - z1\.b}, pn\1, {z0\.b - z1\.b}, {z4\.b - z5\.b}
**	ret
*/
TEST_XN (sel_z0_pn7_z0_z4, svint8x2_t, z0,
	 svsel_s8_x2 (pn7, z0, z4),
	 svsel (pn7, z0, z4))

/*
** sel_z0_pn8_z4_z28:
**	sel	{z0\.b - z1\.b}, pn8, {z4\.b - z5\.b}, {z28\.b - z29\.b}
**	ret
*/
TEST_XN (sel_z0_pn8_z4_z28, svint8x2_t, z0,
	 svsel_s8_x2 (pn8, z4, z28),
	 svsel (pn8, z4, z28))

/*
** sel_z4_pn8_z18_z0:
**	sel	{z4\.b - z5\.b}, pn8, {z18\.b - z19\.b}, {z0\.b - z1\.b}
**	ret
*/
TEST_XN (sel_z4_pn8_z18_z0, svint8x2_t, z4,
	 svsel_s8_x2 (pn8, z18, z0),
	 svsel (pn8, z18, z0))

/*
** sel_z18_pn15_z28_z4:
**	sel	{z18\.b - z19\.b}, pn15, {z28\.b - z29\.b}, {z4\.b - z5\.b}
**	ret
*/
TEST_XN (sel_z18_pn15_z28_z4, svint8x2_t, z18,
	 svsel_s8_x2 (pn15, z28, z4),
	 svsel (pn15, z28, z4))

/*
** sel_z18_pn8_z18_z4:
**	sel	{z18\.b - z19\.b}, pn8, {z18\.b - z19\.b}, {z4\.b - z5\.b}
**	ret
*/
TEST_XN (sel_z18_pn8_z18_z4, svint8x2_t, z18,
	 svsel_s8_x2 (pn8, z18, z4),
	 svsel (pn8, z18, z4))

/*
** sel_z23_pn15_z0_z18:
**	sel	[^\n]+, pn15, {z0\.b - z1\.b}, {z18\.b - z19\.b}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (sel_z23_pn15_z0_z18, svint8x2_t, z23,
	 svsel_s8_x2 (pn15, z0, z18),
	 svsel (pn15, z0, z18))

/*
** sel_z0_pn15_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	sel	{z0\.b - z1\.b}, pn15, {[^}]+}, {z28\.b - z29\.b}
**	ret
*/
TEST_XN (sel_z0_pn15_z23_z28, svint8x2_t, z0,
	 svsel_s8_x2 (pn15, z23, z28),
	 svsel (pn15, z23, z28))

/*
** sel_z0_pn8_z28_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	sel	{z0\.b - z1\.b}, pn8, {z28\.b - z29\.b}, {[^}]+}
**	ret
*/
TEST_XN (sel_z0_pn8_z28_z23, svint8x2_t, z0,
	 svsel_s8_x2 (pn8, z28, z23),
	 svsel (pn8, z28, z23))
