/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** zip_z0_z0:
**	zip	{z0\.h - z3\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_XN (zip_z0_z0, svbfloat16x4_t, z0,
	 svzip_bf16_x4 (z0),
	 svzip (z0))

/*
** zip_z0_z4:
**	zip	{z0\.h - z3\.h}, {z4\.h - z7\.h}
**	ret
*/
TEST_XN (zip_z0_z4, svbfloat16x4_t, z0,
	 svzip_bf16_x4 (z4),
	 svzip (z4))

/*
** zip_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	zip	{z4\.h - z7\.h}, [^\n]+
**	ret
*/
TEST_XN (zip_z4_z18, svbfloat16x4_t, z4,
	 svzip_bf16_x4 (z18),
	 svzip (z18))

/*
** zip_z18_z23:
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
TEST_XN (zip_z18_z23, svbfloat16x4_t, z18,
	 svzip_bf16_x4 (z23),
	 svzip (z23))

/*
** zip_z23_z28:
**	zip	[^\n]+, {z28\.h - z31\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (zip_z23_z28, svbfloat16x4_t, z23,
	 svzip_bf16_x4 (z28),
	 svzip (z28))

/*
** zip_z28_z0:
**	zip	{z28\.h - z31\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_XN (zip_z28_z0, svbfloat16x4_t, z28,
	 svzip_bf16_x4 (z0),
	 svzip (z0))
