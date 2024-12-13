/* { dg-do assemble { target aarch64_asm_fp8_ok } } */
/* { dg-do compile { target { ! aarch64_asm_fp8_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+bf16+fp8"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** cvtnt_mf8_f32_x2_fpm_untied:
**	msr	fpmr, x2
**	fcvtnt	z1\.b, {z4\.s(?:, | - )z5\.s}
**	mov	z0.d, z1.d
**	ret
*/
TEST_DUAL_Z (cvtnt_mf8_f32_x2_fpm_untied, svmfloat8_t, svfloat32x2_t,
	     z0 = svcvtnt_mf8_f32_x2_fpm (z1, z4, fpm0),
	     z0 = svcvtnt_mf8_fpm (z1, z4, fpm0))

/*
** cvtnt_mf8_f32_x2_fpm_tied:
**	msr	fpmr, x2
**	fcvtnt	z0\.b, {z4\.s(?:, | - )z5\.s}
**	ret
*/
TEST_DUAL_Z (cvtnt_mf8_f32_x2_fpm_tied, svmfloat8_t, svfloat32x2_t,
	     z0 = svcvtnt_mf8_f32_x2_fpm (z0, z4, fpm0),
	     z0 = svcvtnt_mf8_fpm (z0, z4, fpm0))
