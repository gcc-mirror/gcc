/* { dg-do assemble { target { aarch64_asm_fp8_ok && aarch64_asm_sme2_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_fp8_ok && aarch64_asm_sme2_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+fp8"

/*
** cvt1_f16_mf8_x2_fpm:
**	msr	fpmr, x0
**	f1cvt	{z0\.h - z1\.h}, z0\.b
**	ret
*/
TEST_X2_WIDE (cvt1_f16_mf8_x2_fpm, svfloat16x2_t, svmfloat8_t,
	      z0_res = svcvt1_f16_mf8_x2_fpm (z0, fpm0),
	      z0_res = svcvt1_f16_x2_fpm (z0, fpm0))

/*
** cvt1_bf16_mf8_x2_fpm:
**	msr	fpmr, x0
**	bf1cvt	{z0\.h - z1\.h}, z0\.b
**	ret
*/
TEST_X2_WIDE (cvt1_bf16_mf8_x2_fpm, svbfloat16x2_t, svmfloat8_t,
	      z0_res = svcvt1_bf16_mf8_x2_fpm (z0, fpm0),
	      z0_res = svcvt1_bf16_x2_fpm (z0, fpm0))

/*
** cvt2_f16_mf8_x2_fpm:
**	msr	fpmr, x0
**	f2cvt	{z0\.h - z1\.h}, z0\.b
**	ret
*/
TEST_X2_WIDE (cvt2_f16_mf8_x2_fpm, svfloat16x2_t, svmfloat8_t,
	      z0_res = svcvt2_f16_mf8_x2_fpm (z0, fpm0),
	      z0_res = svcvt2_f16_x2_fpm (z0, fpm0))

/*
** cvt2_bf16_mf8_x2_fpm:
**	msr	fpmr, x0
**	bf2cvt	{z0\.h - z1\.h}, z0\.b
**	ret
*/
TEST_X2_WIDE (cvt2_bf16_mf8_x2_fpm, svbfloat16x2_t, svmfloat8_t,
	      z0_res = svcvt2_bf16_mf8_x2_fpm (z0, fpm0),
	      z0_res = svcvt2_bf16_x2_fpm (z0, fpm0))
