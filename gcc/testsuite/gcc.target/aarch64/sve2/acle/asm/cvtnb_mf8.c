/* { dg-do assemble { target aarch64_asm_fp8_ok } } */
/* { dg-do compile { target { ! aarch64_asm_fp8_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+bf16+fp8"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** cvtnb_mf8_f32_x2_fpm:
**	msr	fpmr, x2
**	fcvtnb	z0\.b, {z4\.s(?:, | - )z5\.s}
**	ret
*/
TEST_DUAL_Z (cvtnb_mf8_f32_x2_fpm, svmfloat8_t, svfloat32x2_t,
	     z0 = svcvtnb_mf8_f32_x2_fpm (z4, fpm0),
	     z0 = svcvtnb_mf8_fpm (z4, fpm0))
