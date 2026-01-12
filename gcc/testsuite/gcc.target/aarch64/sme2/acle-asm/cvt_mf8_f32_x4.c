/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */
/* { dg-do assemble { target { aarch64_asm_fp8_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_fp8_ok } } } } */

#include "test_sme2_acle.h"
#pragma GCC target "+fp8"

/*
** cvt_z0_z0:
**	msr	fpmr, x0
**	fcvt	z0\.b, {z0\.s - z3\.s}
**	ret
*/
TEST_X4_NARROW (cvt_z0_z0, svfloat32x4_t, svmfloat8_t,
		z0_res = svcvt_mf8_f32_x4_fpm (z0, fpm0),
		z0_res = svcvt_mf8_fpm (z0, fpm0))

/*
** cvt_z0_z4:
**	msr	fpmr, x0
**	fcvt	z0\.b, {z4\.s - z7\.s}
**	ret
*/
TEST_X4_NARROW (cvt_z0_z4, svfloat32x4_t, svmfloat8_t,
		z0_res = svcvt_mf8_f32_x4_fpm (z4, fpm0),
		z0_res = svcvt_mf8_fpm (z4, fpm0))

/*
** cvt_z0_z21:
**	msr	fpmr, x0
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvt	z0\.b, [^\n]+
**	ret
*/
TEST_X4_NARROW (cvt_z0_z21, svfloat32x4_t, svmfloat8_t,
		z0_res = svcvt_mf8_f32_x4_fpm (z21, fpm0),
		z0_res = svcvt_mf8_fpm (z21, fpm0))

/*
** cvt_z25_z26:
**	msr	fpmr, x0
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvt	z25\.b, {z28\.s - z31\.s}
**	ret
*/
TEST_X4_NARROW (cvt_z25_z26, svfloat32x4_t, svmfloat8_t,
		z25 = svcvt_mf8_f32_x4_fpm (z26, fpm0),
		z25 = svcvt_mf8_fpm (z26, fpm0))

/*
** cvt_z25_z0:
**	msr	fpmr, x0
**	fcvt	z25\.b, {z0\.s - z3\.s}
**	ret
*/
TEST_X4_NARROW (cvt_z25_z0, svfloat32x4_t, svmfloat8_t,
	z25 = svcvt_mf8_f32_x4_fpm (z0, fpm0),
	z25 = svcvt_mf8_fpm (z0, fpm0))

/*
** cvt_z22_z16:
**	msr	fpmr, x0
**	fcvt	z22\.b, {z16\.s - z19\.s}
**	ret
*/
TEST_X4_NARROW (cvt_z22_z16, svfloat32x4_t, svmfloat8_t,
	z22_res = svcvt_mf8_f32_x4_fpm (z16, fpm0),
	z22_res = svcvt_mf8_fpm (z16, fpm0))
