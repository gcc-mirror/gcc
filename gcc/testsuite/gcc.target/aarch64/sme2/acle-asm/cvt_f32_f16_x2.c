/* { dg-do assemble { target aarch64_asm_sme-f16f16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-f16f16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme-f16f16"

/*
** cvt_z0_z0:
**	fcvt	{z0\.s - z1\.s}, z0\.h
**	ret
*/
TEST_X2_WIDE (cvt_z0_z0, svfloat32x2_t, svfloat16_t,
	      z0_res = svcvt_f32_f16_x2 (z0),
	      z0_res = svcvt_f32 (z0))

/*
** cvt_z0_z6:
**	fcvt	{z6\.s - z7\.s}, z0\.h
**	ret
*/
TEST_X2_WIDE (cvt_z0_z6, svfloat32x2_t, svfloat16_t,
	      z6 = svcvt_f32_f16_x2 (z0),
	      z6 = svcvt_f32 (z0))

/*
** cvt_z0_z29:
**	fcvt	[^\n]+, z0\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_X2_WIDE (cvt_z0_z29, svfloat32x2_t, svfloat16_t,
	      z29 = svcvt_f32_f16_x2 (z0),
	      z29 = svcvt_f32 (z0))

/*
** cvt_z5_z0:
**	fcvt	{z0\.s - z1\.s}, z5\.h
**	ret
*/
TEST_X2_WIDE (cvt_z5_z0, svfloat32x2_t, svfloat16_t,
	      z0_res = svcvt_f32_f16_x2 (z5),
	      z0_res = svcvt_f32 (z5))

/*
** cvt_z22_z16:
**	fcvt	{z16\.s - z17\.s}, z22\.h
**	ret
*/
TEST_X2_WIDE (cvt_z22_z16, svfloat32x2_t, svfloat16_t,
	      z16 = svcvt_f32_f16_x2 (z22),
	      z16 = svcvt_f32 (z22))
