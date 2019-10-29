/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rbit_u16_m_tied12:
**	rbit	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rbit_u16_m_tied12, svuint16_t,
		z0 = svrbit_u16_m (z0, p0, z0),
		z0 = svrbit_m (z0, p0, z0))

/*
** rbit_u16_m_tied1:
**	rbit	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rbit_u16_m_tied1, svuint16_t,
		z0 = svrbit_u16_m (z0, p0, z1),
		z0 = svrbit_m (z0, p0, z1))

/*
** rbit_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	rbit	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_UNIFORM_Z (rbit_u16_m_tied2, svuint16_t,
		z0 = svrbit_u16_m (z1, p0, z0),
		z0 = svrbit_m (z1, p0, z0))

/*
** rbit_u16_m_untied:
**	movprfx	z0, z2
**	rbit	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rbit_u16_m_untied, svuint16_t,
		z0 = svrbit_u16_m (z2, p0, z1),
		z0 = svrbit_m (z2, p0, z1))

/*
** rbit_u16_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, \1\.h
**	rbit	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_UNIFORM_Z (rbit_u16_z_tied1, svuint16_t,
		z0 = svrbit_u16_z (p0, z0),
		z0 = svrbit_z (p0, z0))

/*
** rbit_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	rbit	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rbit_u16_z_untied, svuint16_t,
		z0 = svrbit_u16_z (p0, z1),
		z0 = svrbit_z (p0, z1))

/*
** rbit_u16_x_tied1:
**	rbit	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rbit_u16_x_tied1, svuint16_t,
		z0 = svrbit_u16_x (p0, z0),
		z0 = svrbit_x (p0, z0))

/*
** rbit_u16_x_untied:
**	rbit	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rbit_u16_x_untied, svuint16_t,
		z0 = svrbit_u16_x (p0, z1),
		z0 = svrbit_x (p0, z1))
