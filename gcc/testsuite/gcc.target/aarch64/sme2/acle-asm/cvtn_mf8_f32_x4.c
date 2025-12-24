/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+fp8"

/*
** cvtn_z0_z0:
**	msr	fpmr, x0
**	fcvtn	z0\.b, {z0\.s - z3\.s}
**	ret
*/
TEST_X4_NARROW (cvtn_z0_z0, svfloat32x4_t, svmfloat8_t,
		z0_res = svcvtn_mf8_f32_x4_fpm (z0, fpm0),
		z0_res = svcvtn_mf8_fpm (z0, fpm0))

/*
** cvtn_z0_z4:
**	msr	fpmr, x0
**	fcvtn	z0\.b, {z4\.s - z7\.s}
**	ret
*/
TEST_X4_NARROW (cvtn_z0_z4, svfloat32x4_t, svmfloat8_t,
		z0_res = svcvtn_mf8_f32_x4_fpm (z4, fpm0),
		z0_res = svcvtn_mf8_fpm (z4, fpm0))

/*
** cvtn_z0_z21:
**	msr	fpmr, x0
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvtn	z0\.b, [^\n]+
**	ret
*/
TEST_X4_NARROW (cvtn_z0_z21, svfloat32x4_t, svmfloat8_t,
		z0_res = svcvtn_mf8_f32_x4_fpm (z21, fpm0),
		z0_res = svcvtn_mf8_fpm (z21, fpm0))

/*
** cvtn_z25_z26:
**	msr	fpmr, x0
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvtn	z25\.b, {z28\.s - z31\.s}
**	ret
*/
TEST_X4_NARROW (cvtn_z25_z26, svfloat32x4_t, svmfloat8_t,
		z25 = svcvtn_mf8_f32_x4_fpm (z26, fpm0),
		z25 = svcvtn_mf8_fpm (z26, fpm0))

/*
** cvtn_z25_z0:
**	msr	fpmr, x0
**	fcvtn	z25\.b, {z0\.s - z3\.s}
**	ret
*/
TEST_X4_NARROW (cvtn_z25_z0, svfloat32x4_t, svmfloat8_t,
	z25 = svcvtn_mf8_f32_x4_fpm (z0, fpm0),
	z25 = svcvtn_mf8_fpm (z0, fpm0))

/*
** cvtn_z22_z16:
**	msr	fpmr, x0
**	fcvtn	z22\.b, {z16\.s - z19\.s}
**	ret
*/
TEST_X4_NARROW (cvtn_z22_z16, svfloat32x4_t, svmfloat8_t,
	z22_res = svcvtn_mf8_f32_x4_fpm (z16, fpm0),
	z22_res = svcvtn_mf8_fpm (z16, fpm0))
