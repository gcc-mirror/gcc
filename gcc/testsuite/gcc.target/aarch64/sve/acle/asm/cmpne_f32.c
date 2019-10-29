/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpne_f32_tied:
**	fcmne	p0\.s, p0/z, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpne_f32_tied, svfloat32_t,
		p0 = svcmpne_f32 (p0, z0, z1),
		p0 = svcmpne (p0, z0, z1))

/*
** cmpne_f32_untied:
**	fcmne	p0\.s, p1/z, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpne_f32_untied, svfloat32_t,
		p0 = svcmpne_f32 (p1, z0, z1),
		p0 = svcmpne (p1, z0, z1))

/*
** cmpne_s4_f32:
**	mov	(z[0-9]+\.s), s4
**	fcmne	p0\.s, p1/z, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_COMPARE_ZD (cmpne_s4_f32, svfloat32_t, float32_t,
		 p0 = svcmpne_n_f32 (p1, z0, d4),
		 p0 = svcmpne (p1, z0, d4))

/*
** cmpne_0_f32:
**	fcmne	p0\.s, p1/z, z0\.s, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmpne_0_f32, svfloat32_t,
		p0 = svcmpne_n_f32 (p1, z0, 0),
		p0 = svcmpne (p1, z0, 0))

/*
** cmpne_1_f32:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	fcmne	p0\.s, p1/z, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpne_1_f32, svfloat32_t,
		p0 = svcmpne_n_f32 (p1, z0, 1),
		p0 = svcmpne (p1, z0, 1))
