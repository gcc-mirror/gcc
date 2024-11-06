/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** acle_f64_tied:
** (
**	facge	p0\.d, p0/z, z1\.d, z0\.d
** |
**	facle	p0\.d, p0/z, z0\.d, z1\.d
** )
**	ret
*/
TEST_COMPARE_Z (acle_f64_tied, svfloat64_t,
		p0 = svacle_f64 (p0, z0, z1),
		p0 = svacle (p0, z0, z1))

/*
** acle_f64_untied:
** (
**	facge	p0\.d, p1/z, z1\.d, z0\.d
** |
**	facle	p0\.d, p1/z, z0\.d, z1\.d
** )
**	ret
*/
TEST_COMPARE_Z (acle_f64_untied, svfloat64_t,
		p0 = svacle_f64 (p1, z0, z1),
		p0 = svacle (p1, z0, z1))

/*
** acle_d4_f64:
**	mov	(z[0-9]+\.d), d4
** (
**	facge	p0\.d, p1/z, \1, z0\.d
** |
**	facle	p0\.d, p1/z, z0\.d, \1
** )
**	ret
*/
TEST_COMPARE_ZD (acle_d4_f64, svfloat64_t, float64_t,
		 p0 = svacle_n_f64 (p1, z0, d4),
		 p0 = svacle (p1, z0, d4))

/*
** acle_0_f64:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
** (
**	facge	p0\.d, p1/z, \1, z0\.d
** |
**	facle	p0\.d, p1/z, z0\.d, z\1\.d
** )
**	ret
*/
TEST_COMPARE_Z (acle_0_f64, svfloat64_t,
		p0 = svacle_n_f64 (p1, z0, 0),
		p0 = svacle (p1, z0, 0))

/*
** acle_1_f64:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
** (
**	facge	p0\.d, p1/z, \1, z0\.d
** |
**	facle	p0\.d, p1/z, z0\.d, \1
** )
**	ret
*/
TEST_COMPARE_Z (acle_1_f64, svfloat64_t,
		p0 = svacle_n_f64 (p1, z0, 1),
		p0 = svacle (p1, z0, 1))
