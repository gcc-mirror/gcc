/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rintn_f64_m_tied12:
**	frintn	z0\.d, p0/m, z0\.d
**	ret
*/
TEST_UNIFORM_Z (rintn_f64_m_tied12, svfloat64_t,
		z0 = svrintn_f64_m (z0, p0, z0),
		z0 = svrintn_m (z0, p0, z0))

/*
** rintn_f64_m_tied1:
**	frintn	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rintn_f64_m_tied1, svfloat64_t,
		z0 = svrintn_f64_m (z0, p0, z1),
		z0 = svrintn_m (z0, p0, z1))

/*
** rintn_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	frintn	z0\.d, p0/m, \1
**	ret
*/
TEST_UNIFORM_Z (rintn_f64_m_tied2, svfloat64_t,
		z0 = svrintn_f64_m (z1, p0, z0),
		z0 = svrintn_m (z1, p0, z0))

/*
** rintn_f64_m_untied:
**	movprfx	z0, z2
**	frintn	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rintn_f64_m_untied, svfloat64_t,
		z0 = svrintn_f64_m (z2, p0, z1),
		z0 = svrintn_m (z2, p0, z1))

/*
** rintn_f64_z_tied1:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, \1
**	frintn	z0\.d, p0/m, \1
**	ret
*/
TEST_UNIFORM_Z (rintn_f64_z_tied1, svfloat64_t,
		z0 = svrintn_f64_z (p0, z0),
		z0 = svrintn_z (p0, z0))

/*
** rintn_f64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	frintn	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rintn_f64_z_untied, svfloat64_t,
		z0 = svrintn_f64_z (p0, z1),
		z0 = svrintn_z (p0, z1))

/*
** rintn_f64_x_tied1:
**	frintn	z0\.d, p0/m, z0\.d
**	ret
*/
TEST_UNIFORM_Z (rintn_f64_x_tied1, svfloat64_t,
		z0 = svrintn_f64_x (p0, z0),
		z0 = svrintn_x (p0, z0))

/*
** rintn_f64_x_untied:
**	frintn	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rintn_f64_x_untied, svfloat64_t,
		z0 = svrintn_f64_x (p0, z1),
		z0 = svrintn_x (p0, z1))

/*
** ptrue_rintn_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_rintn_f64_x_tied1, svfloat64_t,
		z0 = svrintn_f64_x (svptrue_b64 (), z0),
		z0 = svrintn_x (svptrue_b64 (), z0))

/*
** ptrue_rintn_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_rintn_f64_x_untied, svfloat64_t,
		z0 = svrintn_f64_x (svptrue_b64 (), z1),
		z0 = svrintn_x (svptrue_b64 (), z1))
