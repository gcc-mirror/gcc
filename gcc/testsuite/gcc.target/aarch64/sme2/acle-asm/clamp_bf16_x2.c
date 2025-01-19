/* { dg-do assemble { target aarch64_asm_sve-b16b16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve-b16b16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sve-b16b16"

/*
** clamp_z24_z24_z0_z5:
**	bfclamp	{z24\.h - z25\.h}, z0\.h, z5\.h
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z24_z0_z5, svbfloat16x2_t, svbfloat16_t, z24,
		svclamp_single_bf16_x2 (z24, z0, z5),
		svclamp (z24, z0, z5))

/*
** clamp_z24_z28_z5_z7:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	bfclamp	{z24\.h - z25\.h}, z5\.h, z7\.h
** |
**	bfclamp	{z28\.h - z29\.h}, z5\.h, z7\.h
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z28_z5_z7, svbfloat16x2_t, svbfloat16_t, z24,
		svclamp_single_bf16_x2 (z28, z5, z7),
		svclamp (z28, z5, z7))

/*
** clamp_z24_z1_z7_z16:
** (
**	mov	z24\.d, z1\.d
**	mov	z25\.d, z2\.d
** |
**	mov	z25\.d, z2\.d
**	mov	z24\.d, z1\.d
** )
**	bfclamp	{z24\.h - z25\.h}, z7\.h, z16\.h
**	ret
*/
TEST_XN_SINGLE (clamp_z24_z1_z7_z16, svbfloat16x2_t, svbfloat16_t, z24,
		svclamp_single_bf16_x2 (z1, z7, z16),
		svclamp (z1, z7, z16))

/*
** clamp_z1_z24_z16_z23:
**	bfclamp	{z24\.h - z25\.h}, z16\.h, z23\.h
** (
**	mov	z1\.d, z24\.d
**	mov	z2\.d, z25\.d
** |
**	mov	z2\.d, z25\.d
**	mov	z1\.d, z24\.d
** )
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z24_z16_z23, svbfloat16x2_t, svbfloat16_t, z1,
		svclamp_single_bf16_x2 (z24, z16, z23),
		svclamp (z24, z16, z23))

/*
** clamp_z1_z1_z23_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	bfclamp	{z[0-9]+\.h - z[0-9]+\.h}, z23\.h, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (clamp_z1_z1_z23_z0, svbfloat16x2_t, svbfloat16_t, z1,
		svclamp_single_bf16_x2 (z1, z23, z0),
		svclamp (z1, z23, z0))

/*
** clamp_z18_z18_z0_z23:
**	bfclamp	{z18\.h - z19\.h}, z0\.h, z23\.h
**	ret
*/
TEST_XN_SINGLE (clamp_z18_z18_z0_z23, svbfloat16x2_t, svbfloat16_t, z18,
		svclamp_single_bf16_x2 (z18, z0, z23),
		svclamp (z18, z0, z23))

/*
** clamp_awkward:
**	...
**	bfclamp	{z[0-9]+\.h - z[0-9]+\.h}, z[0-9]+\.h, z3\.h
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (clamp_awkward, svbfloat16x2_t, svbfloat16_t,
			z0_res = svclamp_single_bf16_x2 (z1, z0, zn),
			z0_res = svclamp (z1, z0, zn))
