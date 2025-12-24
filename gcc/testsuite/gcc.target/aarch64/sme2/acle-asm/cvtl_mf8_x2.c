/* { dg-do assemble { target { aarch64_asm_fp8_ok && aarch64_asm_sme2_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_fp8_ok && aarch64_asm_sme2_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+fp8"

/*
** cvtl1_f16_mf8_x2_fpm:
**	msr	fpmr, x0
**	f1cvtl	{z0\.h - z1\.h}, z0\.b
**	ret
*/
TEST_X2_WIDE (cvtl1_f16_mf8_x2_fpm, svfloat16x2_t, svmfloat8_t,
	      z0_res = svcvtl1_f16_mf8_x2_fpm (z0, fpm0),
	      z0_res = svcvtl1_f16_x2_fpm (z0, fpm0))

/*
** cvtl1_bf16_mf8_x2_fpm:
**	msr	fpmr, x0
**	bf1cvtl	{z0\.h - z1\.h}, z0\.b
**	ret
*/
TEST_X2_WIDE (cvtl1_bf16_mf8_x2_fpm, svbfloat16x2_t, svmfloat8_t,
	      z0_res = svcvtl1_bf16_mf8_x2_fpm (z0, fpm0),
	      z0_res = svcvtl1_bf16_x2_fpm (z0, fpm0))

/*
** cvtl2_f16_mf8_x2_fpm:
**	msr	fpmr, x0
**	f2cvtl	{z0\.h - z1\.h}, z0\.b
**	ret
*/
TEST_X2_WIDE (cvtl2_f16_mf8_x2_fpm, svfloat16x2_t, svmfloat8_t,
	      z0_res = svcvtl2_f16_mf8_x2_fpm (z0, fpm0),
	      z0_res = svcvtl2_f16_x2_fpm (z0, fpm0))

/*
** cvtl2_bf16_mf8_x2_fpm:
**	msr	fpmr, x0
**	bf2cvtl	{z0\.h - z1\.h}, z0\.b
**	ret
*/
TEST_X2_WIDE (cvtl2_bf16_mf8_x2_fpm, svbfloat16x2_t, svmfloat8_t,
	      z0_res = svcvtl2_bf16_mf8_x2_fpm (z0, fpm0),
	      z0_res = svcvtl2_bf16_x2_fpm (z0, fpm0))
