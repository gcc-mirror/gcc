/* { dg-do assemble { target aarch64_asm_fp8_ok } } */
/* { dg-do compile { target { ! aarch64_asm_fp8_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+bf16+fp8"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** cvtlt1_f16_mf8_fpm:
**	msr	fpmr, x0
**	f1cvtlt	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (cvtlt1_f16_mf8_fpm, svfloat16_t, svmfloat8_t,
	     z0 = svcvtlt1_f16_mf8_fpm (z4, fpm0),
	     z0 = svcvtlt1_f16_fpm (z4, fpm0))

/*
** cvtlt1_bf16_mf8_fpm:
**	msr	fpmr, x0
**	bf1cvtlt	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (cvtlt1_bf16_mf8_fpm, svbfloat16_t, svmfloat8_t,
	     z0 = svcvtlt1_bf16_mf8_fpm (z4, fpm0),
	     z0 = svcvtlt1_bf16_fpm (z4, fpm0))

/*
** cvtlt2_f16_mf8_fpm:
**	msr	fpmr, x0
**	f2cvtlt	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (cvtlt2_f16_mf8_fpm, svfloat16_t, svmfloat8_t,
	     z0 = svcvtlt2_f16_mf8_fpm (z4, fpm0),
	     z0 = svcvtlt2_f16_fpm (z4, fpm0))

/*
** cvtlt2_bf16_mf8_fpm:
**	msr	fpmr, x0
**	bf2cvtlt	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (cvtlt2_bf16_mf8_fpm, svbfloat16_t, svmfloat8_t,
	     z0 = svcvtlt2_bf16_mf8_fpm (z4, fpm0),
	     z0 = svcvtlt2_bf16_fpm (z4, fpm0))
