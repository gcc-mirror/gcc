/* { dg-do assemble { target { aarch64_asm_sme2_ok && aarch64_asm_faminmax_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme2_ok && aarch64_asm_faminmax_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+faminmax"

/*
** amin_z0_z0_z4:
**	famin	{z0\.d - z1\.d}, {z0\.d - z1\.d}, {z4\.d - z5\.d}
**	ret
*/
TEST_XN (amin_z0_z0_z4, svfloat64x2_t, z0,
	 svamin_f64_x2 (z0, z4),
	 svamin (z0, z4))

/*
** amin_z0_z4_z0:
**	famin	{z0\.d - z1\.d}, {z0\.d - z1\.d}, {z4\.d - z5\.d}
**	ret
*/
TEST_XN (amin_z0_z4_z0, svfloat64x2_t, z0,
	 svamin_f64_x2 (z4, z0),
	 svamin (z4, z0))

/*
** amin_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	famin	[^\n]+, {z28\.d - z29\.d}
** |
**	famin	[^\n]+, {z28\.d - z29\.d}
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amin_z0_z4_z28, svfloat64x2_t, z0,
	 svamin_f64_x2 (z4, z28),
	 svamin (z4, z28))

/*
** amin_z18_z18_z4:
**	famin	{z18\.d - z19\.d}, {z18\.d - z19\.d}, {z4\.d - z5\.d}
**	ret
*/
TEST_XN (amin_z18_z18_z4, svfloat64x2_t, z18,
	 svamin_f64_x2 (z18, z4),
	 svamin (z18, z4))

/*
** amin_z23_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	famin	[^\n]+, {z18\.d - z19\.d}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (amin_z23_z23_z18, svfloat64x2_t, z23,
	 svamin_f64_x2 (z23, z18),
	 svamin (z23, z18))

/*
** amin_z28_z28_z0:
**	famin	{z28\.d - z29\.d}, {z28\.d - z29\.d}, {z0\.d - z1\.d}
**	ret
*/
TEST_XN (amin_z28_z28_z0, svfloat64x2_t, z28,
	 svamin_f64_x2 (z28, z0),
	 svamin (z28, z0))

/*
** amin_z0_z0_z18:
**	famin	{z0\.d - z1\.d}, {z0\.d - z1\.d}, {z18\.d - z19\.d}
**	ret
*/
TEST_XN (amin_z0_z0_z18, svfloat64x2_t, z0,
	 svamin_f64_x2 (z0, z18),
	 svamin (z0, z18))

/*
** amin_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	famin	{z4\.d - z5\.d}, {z4\.d - z5\.d}, [^\n]+
** |
**	famin	{z4\.d - z5\.d}, {z4\.d - z5\.d}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (amin_z4_z4_z23, svfloat64x2_t, z4,
	 svamin_f64_x2 (z4, z23),
	 svamin (z4, z23))
