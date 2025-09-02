/* { dg-do assemble { target { aarch64_asm_sme2_ok && aarch64_asm_faminmax_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme2_ok && aarch64_asm_faminmax_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+faminmax"

/*
** amax_z0_z0_z4:
**	famax	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (amax_z0_z0_z4, svfloat32x2_t, z0,
	 svamax_f32_x2 (z0, z4),
	 svamax (z0, z4))

/*
** amax_z0_z4_z0:
**	famax	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (amax_z0_z4_z0, svfloat32x2_t, z0,
	 svamax_f32_x2 (z4, z0),
	 svamax (z4, z0))

/*
** amax_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	[^\n]+, {z28\.s - z29\.s}
** |
**	famax	[^\n]+, {z28\.s - z29\.s}
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amax_z0_z4_z28, svfloat32x2_t, z0,
	 svamax_f32_x2 (z4, z28),
	 svamax (z4, z28))

/*
** amax_z18_z18_z4:
**	famax	{z18\.s - z19\.s}, {z18\.s - z19\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_XN (amax_z18_z18_z4, svfloat32x2_t, z18,
	 svamax_f32_x2 (z18, z4),
	 svamax (z18, z4))

/*
** amax_z23_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	[^\n]+, {z18\.s - z19\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (amax_z23_z23_z18, svfloat32x2_t, z23,
	 svamax_f32_x2 (z23, z18),
	 svamax (z23, z18))

/*
** amax_z28_z28_z0:
**	famax	{z28\.s - z29\.s}, {z28\.s - z29\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_XN (amax_z28_z28_z0, svfloat32x2_t, z28,
	 svamax_f32_x2 (z28, z0),
	 svamax (z28, z0))

/*
** amax_z0_z0_z18:
**	famax	{z0\.s - z1\.s}, {z0\.s - z1\.s}, {z18\.s - z19\.s}
**	ret
*/
TEST_XN (amax_z0_z0_z18, svfloat32x2_t, z0,
	 svamax_f32_x2 (z0, z18),
	 svamax (z0, z18))

/*
** amax_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	famax	{z4\.s - z5\.s}, {z4\.s - z5\.s}, [^\n]+
** |
**	famax	{z4\.s - z5\.s}, {z4\.s - z5\.s}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amax_z4_z4_z23, svfloat32x2_t, z4,
	 svamax_f32_x2 (z4, z23),
	 svamax (z4, z23))
