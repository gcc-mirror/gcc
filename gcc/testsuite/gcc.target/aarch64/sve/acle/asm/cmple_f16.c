/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmple_f16_tied:
** (
**	fcmge	p0\.h, p0/z, z1\.h, z0\.h
** |
**	fcmle	p0\.h, p0/z, z0\.h, z1\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmple_f16_tied, svfloat16_t,
		p0 = svcmple_f16 (p0, z0, z1),
		p0 = svcmple (p0, z0, z1))

/*
** cmple_f16_untied:
** (
**	fcmge	p0\.h, p1/z, z1\.h, z0\.h
** |
**	fcmle	p0\.h, p1/z, z0\.h, z1\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmple_f16_untied, svfloat16_t,
		p0 = svcmple_f16 (p1, z0, z1),
		p0 = svcmple (p1, z0, z1))

/*
** cmple_h4_f16:
**	mov	(z[0-9]+\.h), h4
** (
**	fcmge	p0\.h, p1/z, \1, z0\.h
** |
**	fcmle	p0\.h, p1/z, z0\.h, \1
** )
**	ret
*/
TEST_COMPARE_ZD (cmple_h4_f16, svfloat16_t, float16_t,
		 p0 = svcmple_n_f16 (p1, z0, d4),
		 p0 = svcmple (p1, z0, d4))

/*
** cmple_0_f16:
**	fcmle	p0\.h, p1/z, z0\.h, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmple_0_f16, svfloat16_t,
		p0 = svcmple_n_f16 (p1, z0, 0),
		p0 = svcmple (p1, z0, 0))

/*
** cmple_1_f16:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
** (
**	fcmge	p0\.h, p1/z, \1, z0\.h
** |
**	fcmle	p0\.h, p1/z, z0\.h, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmple_1_f16, svfloat16_t,
		p0 = svcmple_n_f16 (p1, z0, 1),
		p0 = svcmple (p1, z0, 1))
