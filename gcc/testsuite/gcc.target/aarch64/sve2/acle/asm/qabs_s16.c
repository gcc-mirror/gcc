/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qabs_s16_m_tied12:
**	sqabs	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (qabs_s16_m_tied12, svint16_t,
		z0 = svqabs_s16_m (z0, p0, z0),
		z0 = svqabs_m (z0, p0, z0))

/*
** qabs_s16_m_tied1:
**	sqabs	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qabs_s16_m_tied1, svint16_t,
		z0 = svqabs_s16_m (z0, p0, z1),
		z0 = svqabs_m (z0, p0, z1))

/*
** qabs_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqabs	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qabs_s16_m_tied2, svint16_t,
		z0 = svqabs_s16_m (z1, p0, z0),
		z0 = svqabs_m (z1, p0, z0))

/*
** qabs_s16_m_untied:
**	movprfx	z0, z2
**	sqabs	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qabs_s16_m_untied, svint16_t,
		z0 = svqabs_s16_m (z2, p0, z1),
		z0 = svqabs_m (z2, p0, z1))

/*
** qabs_s16_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, \1\.h
**	sqabs	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qabs_s16_z_tied1, svint16_t,
		z0 = svqabs_s16_z (p0, z0),
		z0 = svqabs_z (p0, z0))

/*
** qabs_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	sqabs	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qabs_s16_z_untied, svint16_t,
		z0 = svqabs_s16_z (p0, z1),
		z0 = svqabs_z (p0, z1))

/*
** qabs_s16_x_tied1:
**	sqabs	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (qabs_s16_x_tied1, svint16_t,
		z0 = svqabs_s16_x (p0, z0),
		z0 = svqabs_x (p0, z0))

/*
** qabs_s16_x_untied:
**	sqabs	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qabs_s16_x_untied, svint16_t,
		z0 = svqabs_s16_x (p0, z1),
		z0 = svqabs_x (p0, z1))
