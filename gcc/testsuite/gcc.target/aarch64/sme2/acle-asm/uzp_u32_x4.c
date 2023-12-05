/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** uzp_z0_z0:
**	uzp	{z0\.s - z3\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (uzp_z0_z0, svuint32x4_t, z0,
	 svuzp_u32_x4 (z0),
	 svuzp (z0))

/*
** uzp_z0_z4:
**	uzp	{z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_XN (uzp_z0_z4, svuint32x4_t, z0,
	 svuzp_u32_x4 (z4),
	 svuzp (z4))

/*
** uzp_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uzp	{z4\.s - z7\.s}, [^\n]+
**	ret
*/
TEST_XN (uzp_z4_z18, svuint32x4_t, z4,
	 svuzp_u32_x4 (z18),
	 svuzp (z18))

/*
** uzp_z18_z23:
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
TEST_XN (uzp_z18_z23, svuint32x4_t, z18,
	 svuzp_u32_x4 (z23),
	 svuzp (z23))

/*
** uzp_z23_z28:
**	uzp	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (uzp_z23_z28, svuint32x4_t, z23,
	 svuzp_u32_x4 (z28),
	 svuzp (z28))

/*
** uzp_z28_z0:
**	uzp	{z28\.s - z31\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (uzp_z28_z0, svuint32x4_t, z28,
	 svuzp_u32_x4 (z0),
	 svuzp (z0))
