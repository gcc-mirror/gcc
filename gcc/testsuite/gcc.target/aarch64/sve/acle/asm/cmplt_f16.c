/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmplt_f16_tied:
** (
**	fcmgt	p0\.h, p0/z, z1\.h, z0\.h
** |
**	fcmlt	p0\.h, p0/z, z0\.h, z1\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_f16_tied, svfloat16_t,
		p0 = svcmplt_f16 (p0, z0, z1),
		p0 = svcmplt (p0, z0, z1))

/*
** cmplt_f16_untied:
** (
**	fcmgt	p0\.h, p1/z, z1\.h, z0\.h
** |
**	fcmlt	p0\.h, p1/z, z0\.h, z1\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_f16_untied, svfloat16_t,
		p0 = svcmplt_f16 (p1, z0, z1),
		p0 = svcmplt (p1, z0, z1))

/*
** cmplt_h4_f16:
**	mov	(z[0-9]+\.h), h4
** (
**	fcmgt	p0\.h, p1/z, \1, z0\.h
** |
**	fcmlt	p0\.h, p1/z, z0\.h, \1
** )
**	ret
*/
TEST_COMPARE_ZD (cmplt_h4_f16, svfloat16_t, float16_t,
		 p0 = svcmplt_n_f16 (p1, z0, d4),
		 p0 = svcmplt (p1, z0, d4))

/*
** cmplt_0_f16:
**	fcmlt	p0\.h, p1/z, z0\.h, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmplt_0_f16, svfloat16_t,
		p0 = svcmplt_n_f16 (p1, z0, 0),
		p0 = svcmplt (p1, z0, 0))

/*
** cmplt_1_f16:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
** (
**	fcmgt	p0\.h, p1/z, \1, z0\.h
** |
**	fcmlt	p0\.h, p1/z, z0\.h, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_1_f16, svfloat16_t,
		p0 = svcmplt_n_f16 (p1, z0, 1),
		p0 = svcmplt (p1, z0, 1))
