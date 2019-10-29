/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpgt_f16_tied:
** (
**	fcmgt	p0\.h, p0/z, z0\.h, z1\.h
** |
**	fcmlt	p0\.h, p0/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_f16_tied, svfloat16_t,
		p0 = svcmpgt_f16 (p0, z0, z1),
		p0 = svcmpgt (p0, z0, z1))

/*
** cmpgt_f16_untied:
** (
**	fcmgt	p0\.h, p1/z, z0\.h, z1\.h
** |
**	fcmlt	p0\.h, p1/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_f16_untied, svfloat16_t,
		p0 = svcmpgt_f16 (p1, z0, z1),
		p0 = svcmpgt (p1, z0, z1))

/*
** cmpgt_h4_f16:
**	mov	(z[0-9]+\.h), h4
** (
**	fcmgt	p0\.h, p1/z, z0\.h, \1
** |
**	fcmlt	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_ZD (cmpgt_h4_f16, svfloat16_t, float16_t,
		 p0 = svcmpgt_n_f16 (p1, z0, d4),
		 p0 = svcmpgt (p1, z0, d4))

/*
** cmpgt_0_f16:
**	fcmgt	p0\.h, p1/z, z0\.h, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmpgt_0_f16, svfloat16_t,
		p0 = svcmpgt_n_f16 (p1, z0, 0),
		p0 = svcmpgt (p1, z0, 0))

/*
** cmpgt_1_f16:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
** (
**	fcmgt	p0\.h, p1/z, z0\.h, \1
** |
**	fcmlt	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_1_f16, svfloat16_t,
		p0 = svcmpgt_n_f16 (p1, z0, 1),
		p0 = svcmpgt (p1, z0, 1))
