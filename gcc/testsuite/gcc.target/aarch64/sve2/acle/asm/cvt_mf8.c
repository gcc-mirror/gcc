/* { dg-do assemble { target aarch64_asm_fp8_ok } } */
/* { dg-do compile { target { ! aarch64_asm_fp8_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+bf16+fp8"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** cvt1_f16_mf8_fpm:
**	msr	fpmr, x0
**	f1cvt	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (cvt1_f16_mf8_fpm, svfloat16_t, svmfloat8_t,
	     z0 = svcvt1_f16_mf8_fpm (z4, fpm0), z0 = svcvt1_f16_fpm (z4, fpm0))

/*
** cvt1_bf16_mf8_fpm:
**	msr	fpmr, x0
**	bf1cvt	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (cvt1_bf16_mf8_fpm, svbfloat16_t, svmfloat8_t,
	     z0 = svcvt1_bf16_mf8_fpm (z4, fpm0),
	     z0 = svcvt1_bf16_fpm (z4, fpm0))

/*
** cvt2_f16_mf8_fpm:
**	msr	fpmr, x0
**	f2cvt	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (cvt2_f16_mf8_fpm, svfloat16_t, svmfloat8_t,
	     z0 = svcvt2_f16_mf8_fpm (z4, fpm0), z0 = svcvt2_f16_fpm (z4, fpm0))

/*
** cvt2_bf16_mf8_fpm:
**	msr	fpmr, x0
**	bf2cvt	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (cvt2_bf16_mf8_fpm, svbfloat16_t, svmfloat8_t,
	     z0 = svcvt2_bf16_mf8_fpm (z4, fpm0),
	     z0 = svcvt2_bf16_fpm (z4, fpm0))
