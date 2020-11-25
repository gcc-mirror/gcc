/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** neg_s8_m_tied12:
**	neg	z0\.b, p0/m, z0\.b
**	ret
*/
TEST_UNIFORM_Z (neg_s8_m_tied12, svint8_t,
		z0 = svneg_s8_m (z0, p0, z0),
		z0 = svneg_m (z0, p0, z0))

/*
** neg_s8_m_tied1:
**	neg	z0\.b, p0/m, z1\.b
**	ret
*/
TEST_UNIFORM_Z (neg_s8_m_tied1, svint8_t,
		z0 = svneg_s8_m (z0, p0, z1),
		z0 = svneg_m (z0, p0, z1))

/*
** neg_s8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	neg	z0\.b, p0/m, \1\.b
**	ret
*/
TEST_UNIFORM_Z (neg_s8_m_tied2, svint8_t,
		z0 = svneg_s8_m (z1, p0, z0),
		z0 = svneg_m (z1, p0, z0))

/*
** neg_s8_m_untied:
**	movprfx	z0, z2
**	neg	z0\.b, p0/m, z1\.b
**	ret
*/
TEST_UNIFORM_Z (neg_s8_m_untied, svint8_t,
		z0 = svneg_s8_m (z2, p0, z1),
		z0 = svneg_m (z2, p0, z1))

/*
** neg_s8_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.b, p0/z, \1\.b
**	neg	z0\.b, p0/m, \1\.b
**	ret
*/
TEST_UNIFORM_Z (neg_s8_z_tied1, svint8_t,
		z0 = svneg_s8_z (p0, z0),
		z0 = svneg_z (p0, z0))

/*
** neg_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	neg	z0\.b, p0/m, z1\.b
**	ret
*/
TEST_UNIFORM_Z (neg_s8_z_untied, svint8_t,
		z0 = svneg_s8_z (p0, z1),
		z0 = svneg_z (p0, z1))

/*
** neg_s8_x_tied1:
**	neg	z0\.b, p0/m, z0\.b
**	ret
*/
TEST_UNIFORM_Z (neg_s8_x_tied1, svint8_t,
		z0 = svneg_s8_x (p0, z0),
		z0 = svneg_x (p0, z0))

/*
** neg_s8_x_untied:
**	movprfx	z0, z1
**	neg	z0\.b, p0/m, z1\.b
**	ret
*/
TEST_UNIFORM_Z (neg_s8_x_untied, svint8_t,
		z0 = svneg_s8_x (p0, z1),
		z0 = svneg_x (p0, z1))
