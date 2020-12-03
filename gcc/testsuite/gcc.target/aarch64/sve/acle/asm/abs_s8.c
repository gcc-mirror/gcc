/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abs_s8_m_tied12:
**	abs	z0\.b, p0/m, z0\.b
**	ret
*/
TEST_UNIFORM_Z (abs_s8_m_tied12, svint8_t,
		z0 = svabs_s8_m (z0, p0, z0),
		z0 = svabs_m (z0, p0, z0))

/*
** abs_s8_m_tied1:
**	abs	z0\.b, p0/m, z1\.b
**	ret
*/
TEST_UNIFORM_Z (abs_s8_m_tied1, svint8_t,
		z0 = svabs_s8_m (z0, p0, z1),
		z0 = svabs_m (z0, p0, z1))

/*
** abs_s8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	abs	z0\.b, p0/m, \1\.b
**	ret
*/
TEST_UNIFORM_Z (abs_s8_m_tied2, svint8_t,
		z0 = svabs_s8_m (z1, p0, z0),
		z0 = svabs_m (z1, p0, z0))

/*
** abs_s8_m_untied:
**	movprfx	z0, z2
**	abs	z0\.b, p0/m, z1\.b
**	ret
*/
TEST_UNIFORM_Z (abs_s8_m_untied, svint8_t,
		z0 = svabs_s8_m (z2, p0, z1),
		z0 = svabs_m (z2, p0, z1))

/*
** abs_s8_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.b, p0/z, \1\.b
**	abs	z0\.b, p0/m, \1\.b
**	ret
*/
TEST_UNIFORM_Z (abs_s8_z_tied1, svint8_t,
		z0 = svabs_s8_z (p0, z0),
		z0 = svabs_z (p0, z0))

/*
** abs_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	abs	z0\.b, p0/m, z1\.b
**	ret
*/
TEST_UNIFORM_Z (abs_s8_z_untied, svint8_t,
		z0 = svabs_s8_z (p0, z1),
		z0 = svabs_z (p0, z1))

/*
** abs_s8_x_tied1:
**	abs	z0\.b, p0/m, z0\.b
**	ret
*/
TEST_UNIFORM_Z (abs_s8_x_tied1, svint8_t,
		z0 = svabs_s8_x (p0, z0),
		z0 = svabs_x (p0, z0))

/*
** abs_s8_x_untied:
**	movprfx	z0, z1
**	abs	z0\.b, p0/m, z1\.b
**	ret
*/
TEST_UNIFORM_Z (abs_s8_x_untied, svint8_t,
		z0 = svabs_s8_x (p0, z1),
		z0 = svabs_x (p0, z1))
