/* { dg-do assemble { target aarch64_asm_sve-bfscale_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve-bfscale_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sve-bfscale"

/*
** mul_z0_z0_z4:
**	bfmul	{z0\.h - z1\.h}, {z0\.h - z1\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_XN (mul_z0_z0_z4, svbfloat16x2_t, z0,
	 svmul_bf16_x2 (z0, z4),
	 svmul (z0, z4))

/*
** mul_z0_z4_z0:
**	bfmul	{z0\.h - z1\.h}, {z4\.h - z5\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_XN (mul_z0_z4_z0, svbfloat16x2_t, z0,
	 svmul_bf16_x2 (z4, z0),
	 svmul (z4, z0))

/*
** mul_z0_z4_z28:
**	bfmul	{z0\.h - z1\.h}, {z4\.h - z5\.h}, {z28\.h - z29\.h}
**	ret
*/
TEST_XN (mul_z0_z4_z28, svbfloat16x2_t, z0,
	 svmul_bf16_x2 (z4, z28),
	 svmul (z4, z28))

/*
** mul_z18_z18_z4:
**	bfmul	{z18\.h - z19\.h}, {z18\.h - z19\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_XN (mul_z18_z18_z4, svbfloat16x2_t, z18,
	 svmul_bf16_x2 (z18, z4),
	 svmul (z18, z4))

/*
** mul_z23_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmul	[^\n]+, {z18\.h - z19\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (mul_z23_z23_z18, svbfloat16x2_t, z23,
	 svmul_bf16_x2 (z23, z18),
	 svmul (z23, z18))

/*
** mul_z28_z28_z0:
**	bfmul	{z28\.h - z29\.h}, {z28\.h - z29\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_XN (mul_z28_z28_z0, svbfloat16x2_t, z28,
	 svmul_bf16_x2 (z28, z0),
	 svmul (z28, z0))

/*
** mul_z0_z0_z18:
**	bfmul	{z0\.h - z1\.h}, {z0\.h - z1\.h}, {z18\.h - z19\.h}
**	ret
*/
TEST_XN (mul_z0_z0_z18, svbfloat16x2_t, z0,
	 svmul_bf16_x2 (z0, z18),
	 svmul (z0, z18))

/*
** mul_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmul	{z4\.h - z5\.h}, {z4\.h - z5\.h}, [^\n]+
** |
**	bfmul	{z4\.h - z5\.h}, {z4\.h - z5\.h}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (mul_z4_z4_z23, svbfloat16x2_t, z4,
	 svmul_bf16_x2 (z4, z23),
	 svmul (z4, z23))

/*
** mul_single_z24_z24_z0:
**	bfmul	{z24\.h - z25\.h}, {z24\.h - z25\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (mul_single_z24_z24_z0, svbfloat16x2_t, svbfloat16_t, z24,
		svmul_single_bf16_x2 (z24, z0),
		svmul (z24, z0))

/*
** mul_single_z24_z28_z0:
**	bfmul	{z24\.h - z25\.h}, {z28\.h - z29\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (mul_single_z24_z28_z0, svbfloat16x2_t, svbfloat16_t, z24,
		svmul_single_bf16_x2 (z28, z0),
		svmul (z28, z0))

/*
** mul_single_z24_z1_z0:
** (
**	mov	z30\.d, z1\.d
**	mov	z31\.d, z2\.d
** |
**	mov	z31\.d, z2\.d
**	mov	z30\.d, z1\.d
** )
**	bfmul	{z24\.h - z25\.h}, {z30\.h - z31\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (mul_single_z24_z1_z0, svbfloat16x2_t, svbfloat16_t, z24,
		svmul_single_bf16_x2 (z1, z0),
		svmul (z1, z0))

/*
** mul_single_z1_z24_z0:
**	bfmul	{z30\.h - z31\.h}, {z24\.h - z25\.h}, z0\.h
** (
**	mov	z2\.d, z31\.d
**	mov	z1\.d, z30\.d
** |
**	mov	z1\.d, z30\.d
**	mov	z2\.d, z31\.d
** )
**	ret
*/
TEST_XN_SINGLE (mul_single_z1_z24_z0, svbfloat16x2_t, svbfloat16_t, z1,
		svmul_single_bf16_x2 (z24, z0),
		svmul (z24, z0))

/*
** mul_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmul	({z[0-9]+\.h - z[0-9]+\.h}), \1, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (mul_single_z1_z1_z0, svbfloat16x2_t, svbfloat16_t, z1,
		svmul_single_bf16_x2 (z1, z0),
		svmul (z1, z0))

/*
** mul_single_z18_z18_z0:
**	bfmul	{z18\.h - z19\.h}, {z18\.h - z19\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (mul_single_z18_z18_z0, svbfloat16x2_t, svbfloat16_t, z18,
		svmul_single_bf16_x2 (z18, z0),
		svmul (z18, z0))

/*
** mul_single_awkward:
**	...
**	bfmul	{z0\.h - z1\.h}, {z30\.h - z31\.h}, z[0-9]+\.h
**	ret
*/
TEST_XN_SINGLE_AWKWARD (mul_single_awkward, svbfloat16x2_t, svbfloat16_t,
			z0_res = svmul_single_bf16_x2 (z1, z0),
			z0_res = svmul (z1, z0))

/*
** mul_single_z0_z0_z15:
**	...
**	bfmul	{z0\.h - z1\.h}, {z0\.h - z1\.h}, z15\.h
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (mul_single_z0_z0_z15, svbfloat16x2_t, svbfloat16_t,
		    z0 = svmul_single_bf16_x2 (z0, z15),
		    z0 = svmul (z0, z15))

/*
** mul_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	bfmul	{z24\.h - z25\.h}, {z24\.h - z25\.h}, \1\.h
**	ret
*/
TEST_XN_SINGLE (mul_single_z24_z24_z16, svbfloat16x2_t, svbfloat16_t, z24,
		svmul_single_bf16_x2 (z24, z16),
		svmul (z24, z16))
