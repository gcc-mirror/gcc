/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpne_f16_tied:
**	fcmne	p0\.h, p0/z, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpne_f16_tied, svfloat16_t,
		p0 = svcmpne_f16 (p0, z0, z1),
		p0 = svcmpne (p0, z0, z1))

/*
** cmpne_f16_untied:
**	fcmne	p0\.h, p1/z, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpne_f16_untied, svfloat16_t,
		p0 = svcmpne_f16 (p1, z0, z1),
		p0 = svcmpne (p1, z0, z1))

/*
** cmpne_h4_f16:
**	mov	(z[0-9]+\.h), h4
**	fcmne	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_ZD (cmpne_h4_f16, svfloat16_t, float16_t,
		 p0 = svcmpne_n_f16 (p1, z0, d4),
		 p0 = svcmpne (p1, z0, d4))

/*
** cmpne_0_f16:
**	fcmne	p0\.h, p1/z, z0\.h, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmpne_0_f16, svfloat16_t,
		p0 = svcmpne_n_f16 (p1, z0, 0),
		p0 = svcmpne (p1, z0, 0))

/*
** cmpne_1_f16:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
**	fcmne	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpne_1_f16, svfloat16_t,
		p0 = svcmpne_n_f16 (p1, z0, 1),
		p0 = svcmpne (p1, z0, 1))
