/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** uzp_z0_z0:
**	uzp	{z0\.h - z3\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_XN (uzp_z0_z0, svbfloat16x4_t, z0,
	 svuzp_bf16_x4 (z0),
	 svuzp (z0))

/*
** uzp_z0_z4:
**	uzp	{z0\.h - z3\.h}, {z4\.h - z7\.h}
**	ret
*/
TEST_XN (uzp_z0_z4, svbfloat16x4_t, z0,
	 svuzp_bf16_x4 (z4),
	 svuzp (z4))

/*
** uzp_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uzp	{z4\.h - z7\.h}, [^\n]+
**	ret
*/
TEST_XN (uzp_z4_z18, svbfloat16x4_t, z4,
	 svuzp_bf16_x4 (z18),
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
TEST_XN (uzp_z18_z23, svbfloat16x4_t, z18,
	 svuzp_bf16_x4 (z23),
	 svuzp (z23))

/*
** uzp_z23_z28:
**	uzp	[^\n]+, {z28\.h - z31\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (uzp_z23_z28, svbfloat16x4_t, z23,
	 svuzp_bf16_x4 (z28),
	 svuzp (z28))

/*
** uzp_z28_z0:
**	uzp	{z28\.h - z31\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_XN (uzp_z28_z0, svbfloat16x4_t, z28,
	 svuzp_bf16_x4 (z0),
	 svuzp (z0))
