/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addp_u8_m_tied1:
**	addp	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (addp_u8_m_tied1, svuint8_t,
		z0 = svaddp_u8_m (p0, z0, z1),
		z0 = svaddp_m (p0, z0, z1))

/*
** addp_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	addp	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (addp_u8_m_tied2, svuint8_t,
		z0 = svaddp_u8_m (p0, z1, z0),
		z0 = svaddp_m (p0, z1, z0))

/*
** addp_u8_m_untied:
**	movprfx	z0, z1
**	addp	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (addp_u8_m_untied, svuint8_t,
		z0 = svaddp_u8_m (p0, z1, z2),
		z0 = svaddp_m (p0, z1, z2))

/*
** addp_u8_x_tied1:
**	addp	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (addp_u8_x_tied1, svuint8_t,
		z0 = svaddp_u8_x (p0, z0, z1),
		z0 = svaddp_x (p0, z0, z1))

/*
** addp_u8_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	addp	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (addp_u8_x_tied2, svuint8_t,
		z0 = svaddp_u8_x (p0, z1, z0),
		z0 = svaddp_x (p0, z1, z0))

/*
** addp_u8_x_untied:
**	movprfx	z0, z1
**	addp	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (addp_u8_x_untied, svuint8_t,
		z0 = svaddp_u8_x (p0, z1, z2),
		z0 = svaddp_x (p0, z1, z2))
