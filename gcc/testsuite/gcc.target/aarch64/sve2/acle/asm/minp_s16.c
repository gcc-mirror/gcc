/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** minp_s16_m_tied1:
**	sminp	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (minp_s16_m_tied1, svint16_t,
		z0 = svminp_s16_m (p0, z0, z1),
		z0 = svminp_m (p0, z0, z1))

/*
** minp_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sminp	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (minp_s16_m_tied2, svint16_t,
		z0 = svminp_s16_m (p0, z1, z0),
		z0 = svminp_m (p0, z1, z0))

/*
** minp_s16_m_untied:
**	movprfx	z0, z1
**	sminp	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (minp_s16_m_untied, svint16_t,
		z0 = svminp_s16_m (p0, z1, z2),
		z0 = svminp_m (p0, z1, z2))

/*
** minp_s16_x_tied1:
**	sminp	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (minp_s16_x_tied1, svint16_t,
		z0 = svminp_s16_x (p0, z0, z1),
		z0 = svminp_x (p0, z0, z1))

/*
** minp_s16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sminp	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (minp_s16_x_tied2, svint16_t,
		z0 = svminp_s16_x (p0, z1, z0),
		z0 = svminp_x (p0, z1, z0))

/*
** minp_s16_x_untied:
**	movprfx	z0, z1
**	sminp	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (minp_s16_x_untied, svint16_t,
		z0 = svminp_s16_x (p0, z1, z2),
		z0 = svminp_x (p0, z1, z2))
