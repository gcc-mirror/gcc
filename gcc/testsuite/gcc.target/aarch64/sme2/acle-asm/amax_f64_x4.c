/* { dg-do assemble { target { aarch64_asm_sme2_ok && aarch64_asm_faminmax_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme2_ok && aarch64_asm_faminmax_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+faminmax"

/*
** amax_z0_z0_z4:
**	famax	{z0\.d - z3\.d}, {z0\.d - z3\.d}, {z4\.d - z7\.d}
**	ret
*/
TEST_XN (amax_z0_z0_z4, svfloat64x4_t, z0,
	 svamax_f64_x4 (z0, z4),
	 svamax (z0, z4))

/*
** amax_z0_z4_z0:
**	famax	{z0\.d - z3\.d}, {z0\.d - z3\.d}, {z4\.d - z7\.d}
**	ret
*/
TEST_XN (amax_z0_z4_z0, svfloat64x4_t, z0,
	 svamax_f64_x4 (z4, z0),
	 svamax (z4, z0))

/*
** amax_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	[^\n]+, {z28\.d - z31\.d}
** |
**	famax	[^\n]+, {z28\.d - z31\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amax_z0_z4_z28, svfloat64x4_t, z0,
	 svamax_f64_x4 (z4, z28),
	 svamax (z4, z28))

/*
** amax_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	[^\n]+, {z4\.d - z7\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (amax_z18_z18_z4, svfloat64x4_t, z18,
	 svamax_f64_x4 (z18, z4),
	 svamax (z18, z4))

/*
** amax_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	[^\n]+, {z28\.d - z31\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (amax_z23_z23_z28, svfloat64x4_t, z23,
	 svamax_f64_x4 (z23, z28),
	 svamax (z23, z28))

/*
** amax_z28_z28_z0:
**	famax	{z28\.d - z31\.d}, {z28\.d - z31\.d}, {z0\.d - z3\.d}
**	ret
*/
TEST_XN (amax_z28_z28_z0, svfloat64x4_t, z28,
	 svamax_f64_x4 (z28, z0),
	 svamax (z28, z0))

/*
** amax_z0_z0_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	{z0\.d - z3\.d}, {z0\.d - z3\.d}, [^\n]+
** |
**	famax	{z0\.d - z3\.d}, {z0\.d - z3\.d}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amax_z0_z0_z18, svfloat64x4_t, z0,
	 svamax_f64_x4 (z0, z18),
	 svamax (z0, z18))

/*
** amax_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	{z4\.d - z7\.d}, {z4\.d - z7\.d}, [^\n]+
** |
**	famax	{z4\.d - z7\.d}, {z4\.d - z7\.d}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amax_z4_z4_z23, svfloat64x4_t, z4,
	 svamax_f64_x4 (z4, z23),
	 svamax (z4, z23))
