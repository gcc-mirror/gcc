/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpge_f16_tied:
** (
**	fcmge	p0\.h, p0/z, z0\.h, z1\.h
** |
**	fcmle	p0\.h, p0/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_f16_tied, svfloat16_t,
		p0 = svcmpge_f16 (p0, z0, z1),
		p0 = svcmpge (p0, z0, z1))

/*
** cmpge_f16_untied:
** (
**	fcmge	p0\.h, p1/z, z0\.h, z1\.h
** |
**	fcmle	p0\.h, p1/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_f16_untied, svfloat16_t,
		p0 = svcmpge_f16 (p1, z0, z1),
		p0 = svcmpge (p1, z0, z1))

/*
** cmpge_h4_f16:
**	mov	(z[0-9]+\.h), h4
** (
**	fcmge	p0\.h, p1/z, z0\.h, \1
** |
**	fcmle	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_ZD (cmpge_h4_f16, svfloat16_t, float16_t,
		 p0 = svcmpge_n_f16 (p1, z0, d4),
		 p0 = svcmpge (p1, z0, d4))

/*
** cmpge_0_f16:
**	fcmge	p0\.h, p1/z, z0\.h, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmpge_0_f16, svfloat16_t,
		p0 = svcmpge_n_f16 (p1, z0, 0),
		p0 = svcmpge (p1, z0, 0))

/*
** cmpge_1_f16:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
** (
**	fcmge	p0\.h, p1/z, z0\.h, \1
** |
**	fcmle	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_1_f16, svfloat16_t,
		p0 = svcmpge_n_f16 (p1, z0, 1),
		p0 = svcmpge (p1, z0, 1))
