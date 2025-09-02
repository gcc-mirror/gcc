/* { dg-do assemble { target { aarch64_asm_sme2_ok && aarch64_asm_faminmax_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme2_ok && aarch64_asm_faminmax_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+faminmax"

/*
** amin_z0_z0_z4:
**	famin	{z0\.s - z3\.s}, {z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_XN (amin_z0_z0_z4, svfloat32x4_t, z0,
	 svamin_f32_x4 (z0, z4),
	 svamin (z0, z4))

/*
** amin_z0_z4_z0:
**	famin	{z0\.s - z3\.s}, {z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_XN (amin_z0_z4_z0, svfloat32x4_t, z0,
	 svamin_f32_x4 (z4, z0),
	 svamin (z4, z0))

/*
** amin_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famin	[^\n]+, {z28\.s - z31\.s}
** |
**	famin	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amin_z0_z4_z28, svfloat32x4_t, z0,
	 svamin_f32_x4 (z4, z28),
	 svamin (z4, z28))

/*
** amin_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famin	[^\n]+, {z4\.s - z7\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (amin_z18_z18_z4, svfloat32x4_t, z18,
	 svamin_f32_x4 (z18, z4),
	 svamin (z18, z4))

/*
** amin_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famin	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (amin_z23_z23_z28, svfloat32x4_t, z23,
	 svamin_f32_x4 (z23, z28),
	 svamin (z23, z28))

/*
** amin_z28_z28_z0:
**	famin	{z28\.s - z31\.s}, {z28\.s - z31\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_XN (amin_z28_z28_z0, svfloat32x4_t, z28,
	 svamin_f32_x4 (z28, z0),
	 svamin (z28, z0))

/*
** amin_z0_z0_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famin	{z0\.s - z3\.s}, {z0\.s - z3\.s}, [^\n]+
** |
**	famin	{z0\.s - z3\.s}, {z0\.s - z3\.s}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amin_z0_z0_z18, svfloat32x4_t, z0,
	 svamin_f32_x4 (z0, z18),
	 svamin (z0, z18))

/*
** amin_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famin	{z4\.s - z7\.s}, {z4\.s - z7\.s}, [^\n]+
** |
**	famin	{z4\.s - z7\.s}, {z4\.s - z7\.s}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amin_z4_z4_z23, svfloat32x4_t, z4,
	 svamin_f32_x4 (z4, z23),
	 svamin (z4, z23))
