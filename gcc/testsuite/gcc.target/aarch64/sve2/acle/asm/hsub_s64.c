/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** hsub_s64_m_tied1:
**	shsub	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (hsub_s64_m_tied1, svint64_t,
		z0 = svhsub_s64_m (p0, z0, z1),
		z0 = svhsub_m (p0, z0, z1))

/*
** hsub_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	shsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (hsub_s64_m_tied2, svint64_t,
		z0 = svhsub_s64_m (p0, z1, z0),
		z0 = svhsub_m (p0, z1, z0))

/*
** hsub_s64_m_untied:
**	movprfx	z0, z1
**	shsub	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (hsub_s64_m_untied, svint64_t,
		z0 = svhsub_s64_m (p0, z1, z2),
		z0 = svhsub_m (p0, z1, z2))

/*
** hsub_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	shsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (hsub_x0_s64_m_tied1, svint64_t, int64_t,
		 z0 = svhsub_n_s64_m (p0, z0, x0),
		 z0 = svhsub_m (p0, z0, x0))

/*
** hsub_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	shsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (hsub_x0_s64_m_untied, svint64_t, int64_t,
		 z0 = svhsub_n_s64_m (p0, z1, x0),
		 z0 = svhsub_m (p0, z1, x0))

/*
** hsub_11_s64_m_tied1:
**	mov	(z[0-9]+\.d), #11
**	shsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (hsub_11_s64_m_tied1, svint64_t,
		z0 = svhsub_n_s64_m (p0, z0, 11),
		z0 = svhsub_m (p0, z0, 11))

/*
** hsub_11_s64_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0, z1
**	shsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (hsub_11_s64_m_untied, svint64_t,
		z0 = svhsub_n_s64_m (p0, z1, 11),
		z0 = svhsub_m (p0, z1, 11))

/*
** hsub_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	shsub	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (hsub_s64_z_tied1, svint64_t,
		z0 = svhsub_s64_z (p0, z0, z1),
		z0 = svhsub_z (p0, z0, z1))

/*
** hsub_s64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	shsubr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (hsub_s64_z_tied2, svint64_t,
		z0 = svhsub_s64_z (p0, z1, z0),
		z0 = svhsub_z (p0, z1, z0))

/*
** hsub_s64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	shsub	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	shsubr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (hsub_s64_z_untied, svint64_t,
		z0 = svhsub_s64_z (p0, z1, z2),
		z0 = svhsub_z (p0, z1, z2))

/*
** hsub_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	shsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (hsub_x0_s64_z_tied1, svint64_t, int64_t,
		 z0 = svhsub_n_s64_z (p0, z0, x0),
		 z0 = svhsub_z (p0, z0, x0))

/*
** hsub_x0_s64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	shsub	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	shsubr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (hsub_x0_s64_z_untied, svint64_t, int64_t,
		 z0 = svhsub_n_s64_z (p0, z1, x0),
		 z0 = svhsub_z (p0, z1, x0))

/*
** hsub_11_s64_z_tied1:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0\.d, p0/z, z0\.d
**	shsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (hsub_11_s64_z_tied1, svint64_t,
		z0 = svhsub_n_s64_z (p0, z0, 11),
		z0 = svhsub_z (p0, z0, 11))

/*
** hsub_11_s64_z_untied:
**	mov	(z[0-9]+\.d), #11
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	shsub	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	shsubr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (hsub_11_s64_z_untied, svint64_t,
		z0 = svhsub_n_s64_z (p0, z1, 11),
		z0 = svhsub_z (p0, z1, 11))

/*
** hsub_s64_x_tied1:
**	shsub	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (hsub_s64_x_tied1, svint64_t,
		z0 = svhsub_s64_x (p0, z0, z1),
		z0 = svhsub_x (p0, z0, z1))

/*
** hsub_s64_x_tied2:
**	shsubr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (hsub_s64_x_tied2, svint64_t,
		z0 = svhsub_s64_x (p0, z1, z0),
		z0 = svhsub_x (p0, z1, z0))

/*
** hsub_s64_x_untied:
** (
**	movprfx	z0, z1
**	shsub	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0, z2
**	shsubr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (hsub_s64_x_untied, svint64_t,
		z0 = svhsub_s64_x (p0, z1, z2),
		z0 = svhsub_x (p0, z1, z2))

/*
** hsub_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	shsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (hsub_x0_s64_x_tied1, svint64_t, int64_t,
		 z0 = svhsub_n_s64_x (p0, z0, x0),
		 z0 = svhsub_x (p0, z0, x0))

/*
** hsub_x0_s64_x_untied:
**	mov	z0\.d, x0
**	shsubr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (hsub_x0_s64_x_untied, svint64_t, int64_t,
		 z0 = svhsub_n_s64_x (p0, z1, x0),
		 z0 = svhsub_x (p0, z1, x0))

/*
** hsub_11_s64_x_tied1:
**	mov	(z[0-9]+\.d), #11
**	shsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (hsub_11_s64_x_tied1, svint64_t,
		z0 = svhsub_n_s64_x (p0, z0, 11),
		z0 = svhsub_x (p0, z0, 11))

/*
** hsub_11_s64_x_untied:
**	mov	z0\.d, #11
**	shsubr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (hsub_11_s64_x_untied, svint64_t,
		z0 = svhsub_n_s64_x (p0, z1, 11),
		z0 = svhsub_x (p0, z1, 11))
