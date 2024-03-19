/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** zipq_z0_z0:
**	zip	{z0\.q - z3\.q}, {z0\.q - z3\.q}
**	ret
*/
TEST_XN (zipq_z0_z0, svfloat16x4_t, z0,
	 svzipq_f16_x4 (z0),
	 svzipq (z0))

/*
** zipq_z0_z4:
**	zip	{z0\.q - z3\.q}, {z4\.q - z7\.q}
**	ret
*/
TEST_XN (zipq_z0_z4, svfloat16x4_t, z0,
	 svzipq_f16_x4 (z4),
	 svzipq (z4))

/*
** zipq_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	zip	{z4\.q - z7\.q}, [^\n]+
**	ret
*/
TEST_XN (zipq_z4_z18, svfloat16x4_t, z4,
	 svzipq_f16_x4 (z18),
	 svzipq (z18))

/*
** zipq_z18_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	zip	{z[^\n]+}, {z[^\n]+}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (zipq_z18_z23, svfloat16x4_t, z18,
	 svzipq_f16_x4 (z23),
	 svzipq (z23))

/*
** zipq_z23_z28:
**	zip	[^\n]+, {z28\.q - z31\.q}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (zipq_z23_z28, svfloat16x4_t, z23,
	 svzipq_f16_x4 (z28),
	 svzipq (z28))

/*
** zipq_z28_z0:
**	zip	{z28\.q - z31\.q}, {z0\.q - z3\.q}
**	ret
*/
TEST_XN (zipq_z28_z0, svfloat16x4_t, z28,
	 svzipq_f16_x4 (z0),
	 svzipq (z0))
