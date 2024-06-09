/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** uzpq_z0_z0:
**	uzp	{z0\.q - z3\.q}, {z0\.q - z3\.q}
**	ret
*/
TEST_XN (uzpq_z0_z0, svint32x4_t, z0,
	 svuzpq_s32_x4 (z0),
	 svuzpq (z0))

/*
** uzpq_z0_z4:
**	uzp	{z0\.q - z3\.q}, {z4\.q - z7\.q}
**	ret
*/
TEST_XN (uzpq_z0_z4, svint32x4_t, z0,
	 svuzpq_s32_x4 (z4),
	 svuzpq (z4))

/*
** uzpq_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uzp	{z4\.q - z7\.q}, [^\n]+
**	ret
*/
TEST_XN (uzpq_z4_z18, svint32x4_t, z4,
	 svuzpq_s32_x4 (z18),
	 svuzpq (z18))

/*
** uzpq_z18_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uzp	{z[^\n]+}, {z[^\n]+}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (uzpq_z18_z23, svint32x4_t, z18,
	 svuzpq_s32_x4 (z23),
	 svuzpq (z23))

/*
** uzpq_z23_z28:
**	uzp	[^\n]+, {z28\.q - z31\.q}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (uzpq_z23_z28, svint32x4_t, z23,
	 svuzpq_s32_x4 (z28),
	 svuzpq (z28))

/*
** uzpq_z28_z0:
**	uzp	{z28\.q - z31\.q}, {z0\.q - z3\.q}
**	ret
*/
TEST_XN (uzpq_z28_z0, svint32x4_t, z28,
	 svuzpq_s32_x4 (z0),
	 svuzpq (z0))
