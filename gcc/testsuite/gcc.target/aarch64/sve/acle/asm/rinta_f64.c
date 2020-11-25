/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rinta_f64_m_tied12:
**	frinta	z0\.d, p0/m, z0\.d
**	ret
*/
TEST_UNIFORM_Z (rinta_f64_m_tied12, svfloat64_t,
		z0 = svrinta_f64_m (z0, p0, z0),
		z0 = svrinta_m (z0, p0, z0))

/*
** rinta_f64_m_tied1:
**	frinta	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rinta_f64_m_tied1, svfloat64_t,
		z0 = svrinta_f64_m (z0, p0, z1),
		z0 = svrinta_m (z0, p0, z1))

/*
** rinta_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	frinta	z0\.d, p0/m, \1
**	ret
*/
TEST_UNIFORM_Z (rinta_f64_m_tied2, svfloat64_t,
		z0 = svrinta_f64_m (z1, p0, z0),
		z0 = svrinta_m (z1, p0, z0))

/*
** rinta_f64_m_untied:
**	movprfx	z0, z2
**	frinta	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rinta_f64_m_untied, svfloat64_t,
		z0 = svrinta_f64_m (z2, p0, z1),
		z0 = svrinta_m (z2, p0, z1))

/*
** rinta_f64_z_tied1:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, \1
**	frinta	z0\.d, p0/m, \1
**	ret
*/
TEST_UNIFORM_Z (rinta_f64_z_tied1, svfloat64_t,
		z0 = svrinta_f64_z (p0, z0),
		z0 = svrinta_z (p0, z0))

/*
** rinta_f64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	frinta	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rinta_f64_z_untied, svfloat64_t,
		z0 = svrinta_f64_z (p0, z1),
		z0 = svrinta_z (p0, z1))

/*
** rinta_f64_x_tied1:
**	frinta	z0\.d, p0/m, z0\.d
**	ret
*/
TEST_UNIFORM_Z (rinta_f64_x_tied1, svfloat64_t,
		z0 = svrinta_f64_x (p0, z0),
		z0 = svrinta_x (p0, z0))

/*
** rinta_f64_x_untied:
**	movprfx	z0, z1
**	frinta	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rinta_f64_x_untied, svfloat64_t,
		z0 = svrinta_f64_x (p0, z1),
		z0 = svrinta_x (p0, z1))

/*
** ptrue_rinta_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_rinta_f64_x_tied1, svfloat64_t,
		z0 = svrinta_f64_x (svptrue_b64 (), z0),
		z0 = svrinta_x (svptrue_b64 (), z0))

/*
** ptrue_rinta_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_rinta_f64_x_untied, svfloat64_t,
		z0 = svrinta_f64_x (svptrue_b64 (), z1),
		z0 = svrinta_x (svptrue_b64 (), z1))
