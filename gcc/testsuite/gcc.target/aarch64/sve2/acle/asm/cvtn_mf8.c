/* { dg-do assemble { target aarch64_asm_fp8_ok } } */
/* { dg-do compile { target { ! aarch64_asm_fp8_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+bf16+fp8"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** cvtn_mf8_f16_x2_fpm:
**	msr	fpmr, x2
**	fcvtn	z0\.b, {z4\.h(?:, | - )z5\.h}
**	ret
*/
TEST_DUAL_Z (cvtn_mf8_f16_x2_fpm, svmfloat8_t, svfloat16x2_t,
	     z0 = svcvtn_mf8_f16_x2_fpm (z4, fpm0),
	     z0 = svcvtn_mf8_fpm (z4, fpm0))

/*
** cvtn_mf8_bf16_x2_fpm:
**	msr	fpmr, x2
**	bfcvtn	z0\.b, {z4\.h(?:, | - )z5\.h}
**	ret
*/
TEST_DUAL_Z (cvtn_mf8_bf16_x2_fpm, svmfloat8_t, svbfloat16x2_t,
	     z0 = svcvtn_mf8_bf16_x2_fpm (z4, fpm0),
	     z0 = svcvtn_mf8_fpm (z4, fpm0))
