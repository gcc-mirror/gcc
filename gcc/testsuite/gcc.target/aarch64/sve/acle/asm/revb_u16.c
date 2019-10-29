/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** revb_u16_m_tied12:
**	revb	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (revb_u16_m_tied12, svuint16_t,
		z0 = svrevb_u16_m (z0, p0, z0),
		z0 = svrevb_m (z0, p0, z0))

/*
** revb_u16_m_tied1:
**	revb	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (revb_u16_m_tied1, svuint16_t,
		z0 = svrevb_u16_m (z0, p0, z1),
		z0 = svrevb_m (z0, p0, z1))

/*
** revb_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	revb	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_UNIFORM_Z (revb_u16_m_tied2, svuint16_t,
		z0 = svrevb_u16_m (z1, p0, z0),
		z0 = svrevb_m (z1, p0, z0))

/*
** revb_u16_m_untied:
**	movprfx	z0, z2
**	revb	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (revb_u16_m_untied, svuint16_t,
		z0 = svrevb_u16_m (z2, p0, z1),
		z0 = svrevb_m (z2, p0, z1))

/*
** revb_u16_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, \1\.h
**	revb	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_UNIFORM_Z (revb_u16_z_tied1, svuint16_t,
		z0 = svrevb_u16_z (p0, z0),
		z0 = svrevb_z (p0, z0))

/*
** revb_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	revb	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (revb_u16_z_untied, svuint16_t,
		z0 = svrevb_u16_z (p0, z1),
		z0 = svrevb_z (p0, z1))

/*
** revb_u16_x_tied1:
**	revb	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (revb_u16_x_tied1, svuint16_t,
		z0 = svrevb_u16_x (p0, z0),
		z0 = svrevb_x (p0, z0))

/*
** revb_u16_x_untied:
**	revb	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (revb_u16_x_untied, svuint16_t,
		z0 = svrevb_u16_x (p0, z1),
		z0 = svrevb_x (p0, z1))
