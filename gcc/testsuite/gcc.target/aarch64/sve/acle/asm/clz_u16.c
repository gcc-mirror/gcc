/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clz_u16_m_tied12:
**	clz	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (clz_u16_m_tied12, svuint16_t,
		z0 = svclz_u16_m (z0, p0, z0),
		z0 = svclz_m (z0, p0, z0))

/*
** clz_u16_m_tied1:
**	clz	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (clz_u16_m_tied1, svuint16_t,
		z0 = svclz_u16_m (z0, p0, z1),
		z0 = svclz_m (z0, p0, z1))

/*
** clz_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	clz	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_UNIFORM_Z (clz_u16_m_tied2, svuint16_t,
		z0 = svclz_u16_m (z1, p0, z0),
		z0 = svclz_m (z1, p0, z0))

/*
** clz_u16_m_untied:
**	movprfx	z0, z2
**	clz	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (clz_u16_m_untied, svuint16_t,
		z0 = svclz_u16_m (z2, p0, z1),
		z0 = svclz_m (z2, p0, z1))

/*
** clz_u16_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, \1\.h
**	clz	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_UNIFORM_Z (clz_u16_z_tied1, svuint16_t,
		z0 = svclz_u16_z (p0, z0),
		z0 = svclz_z (p0, z0))

/*
** clz_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	clz	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (clz_u16_z_untied, svuint16_t,
		z0 = svclz_u16_z (p0, z1),
		z0 = svclz_z (p0, z1))

/*
** clz_u16_x_tied1:
**	clz	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (clz_u16_x_tied1, svuint16_t,
		z0 = svclz_u16_x (p0, z0),
		z0 = svclz_x (p0, z0))

/*
** clz_u16_x_untied:
**	clz	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (clz_u16_x_untied, svuint16_t,
		z0 = svclz_u16_x (p0, z1),
		z0 = svclz_x (p0, z1))
