/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** not_s32_m_tied12:
**	not	z0\.s, p0/m, z0\.s
**	ret
*/
TEST_UNIFORM_Z (not_s32_m_tied12, svint32_t,
		z0 = svnot_s32_m (z0, p0, z0),
		z0 = svnot_m (z0, p0, z0))

/*
** not_s32_m_tied1:
**	not	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (not_s32_m_tied1, svint32_t,
		z0 = svnot_s32_m (z0, p0, z1),
		z0 = svnot_m (z0, p0, z1))

/*
** not_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	not	z0\.s, p0/m, \1\.s
**	ret
*/
TEST_UNIFORM_Z (not_s32_m_tied2, svint32_t,
		z0 = svnot_s32_m (z1, p0, z0),
		z0 = svnot_m (z1, p0, z0))

/*
** not_s32_m_untied:
**	movprfx	z0, z2
**	not	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (not_s32_m_untied, svint32_t,
		z0 = svnot_s32_m (z2, p0, z1),
		z0 = svnot_m (z2, p0, z1))

/*
** not_s32_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, \1\.s
**	not	z0\.s, p0/m, \1\.s
**	ret
*/
TEST_UNIFORM_Z (not_s32_z_tied1, svint32_t,
		z0 = svnot_s32_z (p0, z0),
		z0 = svnot_z (p0, z0))

/*
** not_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	not	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (not_s32_z_untied, svint32_t,
		z0 = svnot_s32_z (p0, z1),
		z0 = svnot_z (p0, z1))

/*
** not_s32_x_tied1:
**	not	z0\.s, p0/m, z0\.s
**	ret
*/
TEST_UNIFORM_Z (not_s32_x_tied1, svint32_t,
		z0 = svnot_s32_x (p0, z0),
		z0 = svnot_x (p0, z0))

/*
** not_s32_x_untied:
**	not	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (not_s32_x_untied, svint32_t,
		z0 = svnot_s32_x (p0, z1),
		z0 = svnot_x (p0, z1))
