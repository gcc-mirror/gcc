/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpge_f64_tied:
** (
**	fcmge	p0\.d, p0/z, z0\.d, z1\.d
** |
**	fcmle	p0\.d, p0/z, z1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_f64_tied, svfloat64_t,
		p0 = svcmpge_f64 (p0, z0, z1),
		p0 = svcmpge (p0, z0, z1))

/*
** cmpge_f64_untied:
** (
**	fcmge	p0\.d, p1/z, z0\.d, z1\.d
** |
**	fcmle	p0\.d, p1/z, z1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_f64_untied, svfloat64_t,
		p0 = svcmpge_f64 (p1, z0, z1),
		p0 = svcmpge (p1, z0, z1))

/*
** cmpge_d4_f64:
**	mov	(z[0-9]+\.d), d4
** (
**	fcmge	p0\.d, p1/z, z0\.d, \1
** |
**	fcmle	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_ZD (cmpge_d4_f64, svfloat64_t, float64_t,
		 p0 = svcmpge_n_f64 (p1, z0, d4),
		 p0 = svcmpge (p1, z0, d4))

/*
** cmpge_0_f64:
**	fcmge	p0\.d, p1/z, z0\.d, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmpge_0_f64, svfloat64_t,
		p0 = svcmpge_n_f64 (p1, z0, 0),
		p0 = svcmpge (p1, z0, 0))

/*
** cmpge_1_f64:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
** (
**	fcmge	p0\.d, p1/z, z0\.d, \1
** |
**	fcmle	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_1_f64, svfloat64_t,
		p0 = svcmpge_n_f64 (p1, z0, 1),
		p0 = svcmpge (p1, z0, 1))
