/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** extw_s64_m_tied12:
**	sxtw	z0\.d, p0/m, z0\.d
**	ret
*/
TEST_UNIFORM_Z (extw_s64_m_tied12, svint64_t,
		z0 = svextw_s64_m (z0, p0, z0),
		z0 = svextw_m (z0, p0, z0))

/*
** extw_s64_m_tied1:
**	sxtw	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (extw_s64_m_tied1, svint64_t,
		z0 = svextw_s64_m (z0, p0, z1),
		z0 = svextw_m (z0, p0, z1))

/*
** extw_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sxtw	z0\.d, p0/m, \1
**	ret
*/
TEST_UNIFORM_Z (extw_s64_m_tied2, svint64_t,
		z0 = svextw_s64_m (z1, p0, z0),
		z0 = svextw_m (z1, p0, z0))

/*
** extw_s64_m_untied:
**	movprfx	z0, z2
**	sxtw	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (extw_s64_m_untied, svint64_t,
		z0 = svextw_s64_m (z2, p0, z1),
		z0 = svextw_m (z2, p0, z1))

/*
** extw_s64_z_tied1:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, \1
**	sxtw	z0\.d, p0/m, \1
**	ret
*/
TEST_UNIFORM_Z (extw_s64_z_tied1, svint64_t,
		z0 = svextw_s64_z (p0, z0),
		z0 = svextw_z (p0, z0))

/*
** extw_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	sxtw	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (extw_s64_z_untied, svint64_t,
		z0 = svextw_s64_z (p0, z1),
		z0 = svextw_z (p0, z1))

/*
** extw_s64_x_tied1:
**	sxtw	z0\.d, p0/m, z0\.d
**	ret
*/
TEST_UNIFORM_Z (extw_s64_x_tied1, svint64_t,
		z0 = svextw_s64_x (p0, z0),
		z0 = svextw_x (p0, z0))

/*
** extw_s64_x_untied:
**	movprfx	z0, z1
**	sxtw	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (extw_s64_x_untied, svint64_t,
		z0 = svextw_s64_x (p0, z1),
		z0 = svextw_x (p0, z1))
