/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** revh_s64_m_tied12:
**	revh	z0\.d, p0/m, z0\.d
**	ret
*/
TEST_UNIFORM_Z (revh_s64_m_tied12, svint64_t,
		z0 = svrevh_s64_m (z0, p0, z0),
		z0 = svrevh_m (z0, p0, z0))

/*
** revh_s64_m_tied1:
**	revh	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (revh_s64_m_tied1, svint64_t,
		z0 = svrevh_s64_m (z0, p0, z1),
		z0 = svrevh_m (z0, p0, z1))

/*
** revh_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	revh	z0\.d, p0/m, \1
**	ret
*/
TEST_UNIFORM_Z (revh_s64_m_tied2, svint64_t,
		z0 = svrevh_s64_m (z1, p0, z0),
		z0 = svrevh_m (z1, p0, z0))

/*
** revh_s64_m_untied:
**	movprfx	z0, z2
**	revh	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (revh_s64_m_untied, svint64_t,
		z0 = svrevh_s64_m (z2, p0, z1),
		z0 = svrevh_m (z2, p0, z1))

/*
** revh_s64_z_tied1:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, \1
**	revh	z0\.d, p0/m, \1
**	ret
*/
TEST_UNIFORM_Z (revh_s64_z_tied1, svint64_t,
		z0 = svrevh_s64_z (p0, z0),
		z0 = svrevh_z (p0, z0))

/*
** revh_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	revh	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (revh_s64_z_untied, svint64_t,
		z0 = svrevh_s64_z (p0, z1),
		z0 = svrevh_z (p0, z1))

/*
** revh_s64_x_tied1:
**	revh	z0\.d, p0/m, z0\.d
**	ret
*/
TEST_UNIFORM_Z (revh_s64_x_tied1, svint64_t,
		z0 = svrevh_s64_x (p0, z0),
		z0 = svrevh_x (p0, z0))

/*
** revh_s64_x_untied:
**	movprfx	z0, z1
**	revh	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (revh_s64_x_untied, svint64_t,
		z0 = svrevh_s64_x (p0, z1),
		z0 = svrevh_x (p0, z1))
