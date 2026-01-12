/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */
/* { dg-do assemble { target { aarch64_asm_fp8_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_fp8_ok } } } } */

#include "test_sme2_acle.h"
#pragma GCC target "+fp8"

/*
** cvt_z0_z0:
**	msr	fpmr, x0
**	fcvt	z0\.b, {z0\.h - z1\.h}
**	ret
*/
TEST_X2_NARROW (cvt_z0_z0, svfloat16x2_t, svmfloat8_t,
		z0_res = svcvt_mf8_f16_x2_fpm (z0, fpm0),
		z0_res = svcvt_mf8_fpm (z0, fpm0))

/*
** cvt_z0_z6:
**	msr	fpmr, x0
**	fcvt	z0\.b, {z6\.h - z7\.h}
**	ret
*/
TEST_X2_NARROW (cvt_z0_z6, svfloat16x2_t, svmfloat8_t,
		z0_res = svcvt_mf8_f16_x2_fpm (z6, fpm0),
		z0_res = svcvt_mf8_fpm (z6, fpm0))

/*
** cvt_z0_z29:
**	msr	fpmr, x0
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvt	z0\.b, {z0\.h - z1\.h}
**	ret
*/
TEST_X2_NARROW (cvt_z0_z29, svfloat16x2_t, svmfloat8_t,
		z0_res = svcvt_mf8_f16_x2_fpm (z29, fpm0),
		z0_res = svcvt_mf8_fpm (z29, fpm0))

/*
** cvt_z5_z0:
**	msr	fpmr, x0
**	fcvt	z5\.b, {z0\.h - z1\.h}
**	ret
*/
TEST_X2_NARROW (cvt_z5_z0, svfloat16x2_t, svmfloat8_t,
		z5 = svcvt_mf8_f16_x2_fpm (z0, fpm0),
		z5 = svcvt_mf8_fpm (z0, fpm0))

/*
** cvt_z22_z16:
**	msr	fpmr, x0
**	fcvt	z22\.b, {z16\.h - z17\.h}
**	ret
*/
TEST_X2_NARROW (cvt_z22_z16, svfloat16x2_t, svmfloat8_t,
		z22 = svcvt_mf8_f16_x2_fpm (z16, fpm0),
		z22 = svcvt_mf8_fpm (z16, fpm0))
