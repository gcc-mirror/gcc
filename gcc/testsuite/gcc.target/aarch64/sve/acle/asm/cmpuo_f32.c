/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpuo_f32_tied:
**	fcmuo	p0\.s, p0/z, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpuo_f32_tied, svfloat32_t,
		p0 = svcmpuo_f32 (p0, z0, z1),
		p0 = svcmpuo (p0, z0, z1))

/*
** cmpuo_f32_untied:
**	fcmuo	p0\.s, p1/z, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpuo_f32_untied, svfloat32_t,
		p0 = svcmpuo_f32 (p1, z0, z1),
		p0 = svcmpuo (p1, z0, z1))

/*
** cmpuo_s4_f32:
**	mov	(z[0-9]+\.s), s4
**	fcmuo	p0\.s, p1/z, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_COMPARE_ZD (cmpuo_s4_f32, svfloat32_t, float32_t,
		 p0 = svcmpuo_n_f32 (p1, z0, d4),
		 p0 = svcmpuo (p1, z0, d4))

/*
** cmpuo_0_f32:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	fcmuo	p0\.s, p1/z, (z0\.s, z\1\.s|z\1\.s, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpuo_0_f32, svfloat32_t,
		p0 = svcmpuo_n_f32 (p1, z0, 0),
		p0 = svcmpuo (p1, z0, 0))

/*
** cmpuo_1_f32:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	fcmuo	p0\.s, p1/z, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpuo_1_f32, svfloat32_t,
		p0 = svcmpuo_n_f32 (p1, z0, 1),
		p0 = svcmpuo (p1, z0, 1))
