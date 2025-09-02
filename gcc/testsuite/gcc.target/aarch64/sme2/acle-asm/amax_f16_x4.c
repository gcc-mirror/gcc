/* { dg-do assemble { target { aarch64_asm_sme2_ok && aarch64_asm_faminmax_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme2_ok && aarch64_asm_faminmax_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+faminmax"

/*
** amax_z0_z0_z4:
**	famax	{z0\.h - z3\.h}, {z0\.h - z3\.h}, {z4\.h - z7\.h}
**	ret
*/
TEST_XN (amax_z0_z0_z4, svfloat16x4_t, z0,
	 svamax_f16_x4 (z0, z4),
	 svamax (z0, z4))

/*
** amax_z0_z4_z0:
**	famax	{z0\.h - z3\.h}, {z0\.h - z3\.h}, {z4\.h - z7\.h}
**	ret
*/
TEST_XN (amax_z0_z4_z0, svfloat16x4_t, z0,
	 svamax_f16_x4 (z4, z0),
	 svamax (z4, z0))

/*
** amax_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	[^\n]+, {z28\.h - z31\.h}
** |
**	famax	[^\n]+, {z28\.h - z31\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amax_z0_z4_z28, svfloat16x4_t, z0,
	 svamax_f16_x4 (z4, z28),
	 svamax (z4, z28))

/*
** amax_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	[^\n]+, {z4\.h - z7\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (amax_z18_z18_z4, svfloat16x4_t, z18,
	 svamax_f16_x4 (z18, z4),
	 svamax (z18, z4))

/*
** amax_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	[^\n]+, {z28\.h - z31\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (amax_z23_z23_z28, svfloat16x4_t, z23,
	 svamax_f16_x4 (z23, z28),
	 svamax (z23, z28))

/*
** amax_z28_z28_z0:
**	famax	{z28\.h - z31\.h}, {z28\.h - z31\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_XN (amax_z28_z28_z0, svfloat16x4_t, z28,
	 svamax_f16_x4 (z28, z0),
	 svamax (z28, z0))

/*
** amax_z0_z0_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	{z0\.h - z3\.h}, {z0\.h - z3\.h}, [^\n]+
** |
**	famax	{z0\.h - z3\.h}, {z0\.h - z3\.h}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amax_z0_z0_z18, svfloat16x4_t, z0,
	 svamax_f16_x4 (z0, z18),
	 svamax (z0, z18))

/*
** amax_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	{z4\.h - z7\.h}, {z4\.h - z7\.h}, [^\n]+
** |
**	famax	{z4\.h - z7\.h}, {z4\.h - z7\.h}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amax_z4_z4_z23, svfloat16x4_t, z4,
	 svamax_f16_x4 (z4, z23),
	 svamax (z4, z23))
