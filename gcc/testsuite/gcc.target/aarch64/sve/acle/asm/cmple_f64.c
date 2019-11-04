/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmple_f64_tied:
** (
**	fcmge	p0\.d, p0/z, z1\.d, z0\.d
** |
**	fcmle	p0\.d, p0/z, z0\.d, z1\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmple_f64_tied, svfloat64_t,
		p0 = svcmple_f64 (p0, z0, z1),
		p0 = svcmple (p0, z0, z1))

/*
** cmple_f64_untied:
** (
**	fcmge	p0\.d, p1/z, z1\.d, z0\.d
** |
**	fcmle	p0\.d, p1/z, z0\.d, z1\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmple_f64_untied, svfloat64_t,
		p0 = svcmple_f64 (p1, z0, z1),
		p0 = svcmple (p1, z0, z1))

/*
** cmple_d4_f64:
**	mov	(z[0-9]+\.d), d4
** (
**	fcmge	p0\.d, p1/z, \1, z0\.d
** |
**	fcmle	p0\.d, p1/z, z0\.d, \1
** )
**	ret
*/
TEST_COMPARE_ZD (cmple_d4_f64, svfloat64_t, float64_t,
		 p0 = svcmple_n_f64 (p1, z0, d4),
		 p0 = svcmple (p1, z0, d4))

/*
** cmple_0_f64:
**	fcmle	p0\.d, p1/z, z0\.d, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmple_0_f64, svfloat64_t,
		p0 = svcmple_n_f64 (p1, z0, 0),
		p0 = svcmple (p1, z0, 0))

/*
** cmple_1_f64:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
** (
**	fcmge	p0\.d, p1/z, \1, z0\.d
** |
**	fcmle	p0\.d, p1/z, z0\.d, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmple_1_f64, svfloat64_t,
		p0 = svcmple_n_f64 (p1, z0, 1),
		p0 = svcmple (p1, z0, 1))
