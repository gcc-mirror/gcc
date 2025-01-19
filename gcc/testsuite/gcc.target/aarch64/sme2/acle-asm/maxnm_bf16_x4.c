/* { dg-do assemble { target aarch64_asm_sve-b16b16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve-b16b16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sve-b16b16"

/*
** maxnm_z0_z0_z4:
**	bfmaxnm	{z0\.h - z3\.h}, {z0\.h - z3\.h}, {z4\.h - z7\.h}
**	ret
*/
TEST_XN (maxnm_z0_z0_z4, svbfloat16x4_t, z0,
	 svmaxnm_bf16_x4 (z0, z4),
	 svmaxnm (z0, z4))

/*
** maxnm_z0_z4_z0:
**	bfmaxnm	{z0\.h - z3\.h}, {z0\.h - z3\.h}, {z4\.h - z7\.h}
**	ret
*/
TEST_XN (maxnm_z0_z4_z0, svbfloat16x4_t, z0,
	 svmaxnm_bf16_x4 (z4, z0),
	 svmaxnm (z4, z0))

/*
** maxnm_z0_z4_z28:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmaxnm	[^\n]+, {z28\.h - z31\.h}
** |
**	bfmaxnm	[^\n]+, {z28\.h - z31\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (maxnm_z0_z4_z28, svbfloat16x4_t, z0,
	 svmaxnm_bf16_x4 (z4, z28),
	 svmaxnm (z4, z28))

/*
** maxnm_z18_z18_z4:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmaxnm	[^\n]+, {z4\.h - z7\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (maxnm_z18_z18_z4, svbfloat16x4_t, z18,
	 svmaxnm_bf16_x4 (z18, z4),
	 svmaxnm (z18, z4))

/*
** maxnm_z23_z23_z28:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmaxnm	[^\n]+, {z28\.h - z31\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN (maxnm_z23_z23_z28, svbfloat16x4_t, z23,
	 svmaxnm_bf16_x4 (z23, z28),
	 svmaxnm (z23, z28))

/*
** maxnm_z28_z28_z0:
**	bfmaxnm	{z28\.h - z31\.h}, {z28\.h - z31\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_XN (maxnm_z28_z28_z0, svbfloat16x4_t, z28,
	 svmaxnm_bf16_x4 (z28, z0),
	 svmaxnm (z28, z0))

/*
** maxnm_z0_z0_z18:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmaxnm	{z0\.h - z3\.h}, {z0\.h - z3\.h}, [^\n]+
** |
**	bfmaxnm	{z0\.h - z3\.h}, {z0\.h - z3\.h}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (maxnm_z0_z0_z18, svbfloat16x4_t, z0,
	 svmaxnm_bf16_x4 (z0, z18),
	 svmaxnm (z0, z18))

/*
** maxnm_z4_z4_z23:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmaxnm	{z4\.h - z7\.h}, {z4\.h - z7\.h}, [^\n]+
** |
**	bfmaxnm	{z4\.h - z7\.h}, {z4\.h - z7\.h}, [^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN (maxnm_z4_z4_z23, svbfloat16x4_t, z4,
	 svmaxnm_bf16_x4 (z4, z23),
	 svmaxnm (z4, z23))

/*
** maxnm_single_z24_z24_z0:
**	bfmaxnm	{z24\.h - z27\.h}, {z24\.h - z27\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z24_z24_z0, svbfloat16x4_t, svbfloat16_t, z24,
		svmaxnm_single_bf16_x4 (z24, z0),
		svmaxnm (z24, z0))

/*
** maxnm_single_z24_z28_z0:
** (
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmaxnm	{z24\.h - z27\.h}, {z24\.h - z27\.h}, z0\.h
** |
**	bfmaxnm	{z28\.h - z31\.h}, {z28\.h - z31\.h}, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
** )
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z24_z28_z0, svbfloat16x4_t, svbfloat16_t, z24,
		svmaxnm_single_bf16_x4 (z28, z0),
		svmaxnm (z28, z0))

/*
** maxnm_single_z24_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmaxnm	{z24\.h - z27\.h}, {z24\.h - z27\.h}, z0\.h
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z24_z1_z0, svbfloat16x4_t, svbfloat16_t, z24,
		svmaxnm_single_bf16_x4 (z1, z0),
		svmaxnm (z1, z0))

/*
** maxnm_single_z1_z24_z0:
**	bfmaxnm	{z24\.h - z27\.h}, {z24\.h - z27\.h}, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z1_z24_z0, svbfloat16x4_t, svbfloat16_t, z1,
		svmaxnm_single_bf16_x4 (z24, z0),
		svmaxnm (z24, z0))

/*
** maxnm_single_z1_z1_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmaxnm	({z[0-9]+\.h - z[0-9]+\.h}), \1, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z1_z1_z0, svbfloat16x4_t, svbfloat16_t, z1,
		svmaxnm_single_bf16_x4 (z1, z0),
		svmaxnm (z1, z0))

/*
** maxnm_single_z18_z18_z0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmaxnm	[^\n]+, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z18_z18_z0, svbfloat16x4_t, svbfloat16_t, z18,
		svmaxnm_single_bf16_x4 (z18, z0),
		svmaxnm (z18, z0))

/*
** maxnm_single_awkward:
**	...
**	bfmaxnm	({z[0-9]+\.h - z[0-9]+\.h}), \1, z[0-9]+\.h
**	...
**	ret
*/
TEST_XN_SINGLE_AWKWARD (maxnm_single_awkward, svbfloat16x4_t, svbfloat16_t,
			z0_res = svmaxnm_single_bf16_x4 (z1, z0),
			z0_res = svmaxnm (z1, z0))

/*
** maxnm_single_z0_z0_z15:
**	...
**	bfmaxnm	{z0\.h - z3\.h}, {z0\.h - z3\.h}, z15\.h
**	...
**	ret
*/
TEST_XN_SINGLE_Z15 (maxnm_single_z0_z0_z15, svbfloat16x4_t, svbfloat16_t,
		    z0 = svmaxnm_single_bf16_x4 (z0, z15),
		    z0 = svmaxnm (z0, z15))

/*
** maxnm_single_z24_z24_z16:
**	mov	(z[0-7])\.d, z16\.d
**	bfmaxnm	{z24\.h - z27\.h}, {z24\.h - z27\.h}, \1\.h
**	ret
*/
TEST_XN_SINGLE (maxnm_single_z24_z24_z16, svbfloat16x4_t, svbfloat16_t, z24,
		svmaxnm_single_bf16_x4 (z24, z16),
		svmaxnm (z24, z16))
