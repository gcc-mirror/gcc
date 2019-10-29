/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpgt_f64_tied:
** (
**	fcmgt	p0\.d, p0/z, z0\.d, z1\.d
** |
**	fcmlt	p0\.d, p0/z, z1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_f64_tied, svfloat64_t,
		p0 = svcmpgt_f64 (p0, z0, z1),
		p0 = svcmpgt (p0, z0, z1))

/*
** cmpgt_f64_untied:
** (
**	fcmgt	p0\.d, p1/z, z0\.d, z1\.d
** |
**	fcmlt	p0\.d, p1/z, z1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_f64_untied, svfloat64_t,
		p0 = svcmpgt_f64 (p1, z0, z1),
		p0 = svcmpgt (p1, z0, z1))

/*
** cmpgt_d4_f64:
**	mov	(z[0-9]+\.d), d4
** (
**	fcmgt	p0\.d, p1/z, z0\.d, \1
** |
**	fcmlt	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_ZD (cmpgt_d4_f64, svfloat64_t, float64_t,
		 p0 = svcmpgt_n_f64 (p1, z0, d4),
		 p0 = svcmpgt (p1, z0, d4))

/*
** cmpgt_0_f64:
**	fcmgt	p0\.d, p1/z, z0\.d, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmpgt_0_f64, svfloat64_t,
		p0 = svcmpgt_n_f64 (p1, z0, 0),
		p0 = svcmpgt (p1, z0, 0))

/*
** cmpgt_1_f64:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
** (
**	fcmgt	p0\.d, p1/z, z0\.d, \1
** |
**	fcmlt	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_1_f64, svfloat64_t,
		p0 = svcmpgt_n_f64 (p1, z0, 1),
		p0 = svcmpgt (p1, z0, 1))
