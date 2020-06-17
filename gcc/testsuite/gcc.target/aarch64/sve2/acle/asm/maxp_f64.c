/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** maxp_f64_m_tied1:
**	fmaxp	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (maxp_f64_m_tied1, svfloat64_t,
		z0 = svmaxp_f64_m (p0, z0, z1),
		z0 = svmaxp_m (p0, z0, z1))

/*
** maxp_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fmaxp	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (maxp_f64_m_tied2, svfloat64_t,
		z0 = svmaxp_f64_m (p0, z1, z0),
		z0 = svmaxp_m (p0, z1, z0))

/*
** maxp_f64_m_untied:
**	movprfx	z0, z1
**	fmaxp	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (maxp_f64_m_untied, svfloat64_t,
		z0 = svmaxp_f64_m (p0, z1, z2),
		z0 = svmaxp_m (p0, z1, z2))

/*
** maxp_f64_x_tied1:
**	fmaxp	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (maxp_f64_x_tied1, svfloat64_t,
		z0 = svmaxp_f64_x (p0, z0, z1),
		z0 = svmaxp_x (p0, z0, z1))

/*
** maxp_f64_x_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fmaxp	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (maxp_f64_x_tied2, svfloat64_t,
		z0 = svmaxp_f64_x (p0, z1, z0),
		z0 = svmaxp_x (p0, z1, z0))

/*
** maxp_f64_x_untied:
**	movprfx	z0, z1
**	fmaxp	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (maxp_f64_x_untied, svfloat64_t,
		z0 = svmaxp_f64_x (p0, z1, z2),
		z0 = svmaxp_x (p0, z1, z2))

/*
** ptrue_maxp_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_maxp_f64_x_tied1, svfloat64_t,
		z0 = svmaxp_f64_x (svptrue_b64 (), z0, z1),
		z0 = svmaxp_x (svptrue_b64 (), z0, z1))

/*
** ptrue_maxp_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_maxp_f64_x_tied2, svfloat64_t,
		z0 = svmaxp_f64_x (svptrue_b64 (), z1, z0),
		z0 = svmaxp_x (svptrue_b64 (), z1, z0))

/*
** ptrue_maxp_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_maxp_f64_x_untied, svfloat64_t,
		z0 = svmaxp_f64_x (svptrue_b64 (), z1, z2),
		z0 = svmaxp_x (svptrue_b64 (), z1, z2))
