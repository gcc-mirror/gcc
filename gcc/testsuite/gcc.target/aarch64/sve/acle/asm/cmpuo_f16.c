/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpuo_f16_tied:
**	fcmuo	p0\.h, p0/z, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpuo_f16_tied, svfloat16_t,
		p0 = svcmpuo_f16 (p0, z0, z1),
		p0 = svcmpuo (p0, z0, z1))

/*
** cmpuo_f16_untied:
**	fcmuo	p0\.h, p1/z, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpuo_f16_untied, svfloat16_t,
		p0 = svcmpuo_f16 (p1, z0, z1),
		p0 = svcmpuo (p1, z0, z1))

/*
** cmpuo_h4_f16:
**	mov	(z[0-9]+\.h), h4
**	fcmuo	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_ZD (cmpuo_h4_f16, svfloat16_t, float16_t,
		 p0 = svcmpuo_n_f16 (p1, z0, d4),
		 p0 = svcmpuo (p1, z0, d4))

/*
** cmpuo_0_f16:
**	mov	(z[0-9]+\.h), #0
**	fcmuo	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpuo_0_f16, svfloat16_t,
		p0 = svcmpuo_n_f16 (p1, z0, 0),
		p0 = svcmpuo (p1, z0, 0))

/*
** cmpuo_1_f16:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
**	fcmuo	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpuo_1_f16, svfloat16_t,
		p0 = svcmpuo_n_f16 (p1, z0, 1),
		p0 = svcmpuo (p1, z0, 1))
