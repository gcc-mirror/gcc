/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mla_u64_m_tied1:
**	mla	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mla_u64_m_tied1, svuint64_t,
		z0 = svmla_u64_m (p0, z0, z1, z2),
		z0 = svmla_m (p0, z0, z1, z2))

/*
** mla_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	mla	z0\.d, p0/m, \1, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mla_u64_m_tied2, svuint64_t,
		z0 = svmla_u64_m (p0, z1, z0, z2),
		z0 = svmla_m (p0, z1, z0, z2))

/*
** mla_u64_m_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	mla	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mla_u64_m_tied3, svuint64_t,
		z0 = svmla_u64_m (p0, z1, z2, z0),
		z0 = svmla_m (p0, z1, z2, z0))

/*
** mla_u64_m_untied:
**	movprfx	z0, z1
**	mla	z0\.d, p0/m, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (mla_u64_m_untied, svuint64_t,
		z0 = svmla_u64_m (p0, z1, z2, z3),
		z0 = svmla_m (p0, z1, z2, z3))

/*
** mla_x0_u64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	mla	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (mla_x0_u64_m_tied1, svuint64_t, uint64_t,
		 z0 = svmla_n_u64_m (p0, z0, z1, x0),
		 z0 = svmla_m (p0, z0, z1, x0))

/*
** mla_x0_u64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	mla	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (mla_x0_u64_m_untied, svuint64_t, uint64_t,
		 z0 = svmla_n_u64_m (p0, z1, z2, x0),
		 z0 = svmla_m (p0, z1, z2, x0))

/*
** mla_11_u64_m_tied1:
**	mov	(z[0-9]+\.d), #11
**	mla	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mla_11_u64_m_tied1, svuint64_t,
		z0 = svmla_n_u64_m (p0, z0, z1, 11),
		z0 = svmla_m (p0, z0, z1, 11))

/*
** mla_11_u64_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0, z1
**	mla	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mla_11_u64_m_untied, svuint64_t,
		z0 = svmla_n_u64_m (p0, z1, z2, 11),
		z0 = svmla_m (p0, z1, z2, 11))

/*
** mla_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	mla	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mla_u64_z_tied1, svuint64_t,
		z0 = svmla_u64_z (p0, z0, z1, z2),
		z0 = svmla_z (p0, z0, z1, z2))

/*
** mla_u64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	mad	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mla_u64_z_tied2, svuint64_t,
		z0 = svmla_u64_z (p0, z1, z0, z2),
		z0 = svmla_z (p0, z1, z0, z2))

/*
** mla_u64_z_tied3:
**	movprfx	z0\.d, p0/z, z0\.d
**	mad	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mla_u64_z_tied3, svuint64_t,
		z0 = svmla_u64_z (p0, z1, z2, z0),
		z0 = svmla_z (p0, z1, z2, z0))

/*
** mla_u64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	mla	z0\.d, p0/m, z2\.d, z3\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	mad	z0\.d, p0/m, z3\.d, z1\.d
** |
**	movprfx	z0\.d, p0/z, z3\.d
**	mad	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (mla_u64_z_untied, svuint64_t,
		z0 = svmla_u64_z (p0, z1, z2, z3),
		z0 = svmla_z (p0, z1, z2, z3))

/*
** mla_x0_u64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	mla	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (mla_x0_u64_z_tied1, svuint64_t, uint64_t,
		 z0 = svmla_n_u64_z (p0, z0, z1, x0),
		 z0 = svmla_z (p0, z0, z1, x0))

/*
** mla_x0_u64_z_tied2:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	mad	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (mla_x0_u64_z_tied2, svuint64_t, uint64_t,
		 z0 = svmla_n_u64_z (p0, z1, z0, x0),
		 z0 = svmla_z (p0, z1, z0, x0))

/*
** mla_x0_u64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	mla	z0\.d, p0/m, z2\.d, \1
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	mad	z0\.d, p0/m, \1, z1\.d
** |
**	movprfx	z0\.d, p0/z, \1
**	mad	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (mla_x0_u64_z_untied, svuint64_t, uint64_t,
		 z0 = svmla_n_u64_z (p0, z1, z2, x0),
		 z0 = svmla_z (p0, z1, z2, x0))

/*
** mla_11_u64_z_tied1:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0\.d, p0/z, z0\.d
**	mla	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mla_11_u64_z_tied1, svuint64_t,
		z0 = svmla_n_u64_z (p0, z0, z1, 11),
		z0 = svmla_z (p0, z0, z1, 11))

/*
** mla_11_u64_z_tied2:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0\.d, p0/z, z0\.d
**	mad	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mla_11_u64_z_tied2, svuint64_t,
		z0 = svmla_n_u64_z (p0, z1, z0, 11),
		z0 = svmla_z (p0, z1, z0, 11))

/*
** mla_11_u64_z_untied:
**	mov	(z[0-9]+\.d), #11
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	mla	z0\.d, p0/m, z2\.d, \1
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	mad	z0\.d, p0/m, \1, z1\.d
** |
**	movprfx	z0\.d, p0/z, \1
**	mad	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (mla_11_u64_z_untied, svuint64_t,
		z0 = svmla_n_u64_z (p0, z1, z2, 11),
		z0 = svmla_z (p0, z1, z2, 11))

/*
** mla_u64_x_tied1:
**	mla	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mla_u64_x_tied1, svuint64_t,
		z0 = svmla_u64_x (p0, z0, z1, z2),
		z0 = svmla_x (p0, z0, z1, z2))

/*
** mla_u64_x_tied2:
**	mad	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mla_u64_x_tied2, svuint64_t,
		z0 = svmla_u64_x (p0, z1, z0, z2),
		z0 = svmla_x (p0, z1, z0, z2))

/*
** mla_u64_x_tied3:
**	mad	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mla_u64_x_tied3, svuint64_t,
		z0 = svmla_u64_x (p0, z1, z2, z0),
		z0 = svmla_x (p0, z1, z2, z0))

/*
** mla_u64_x_untied:
** (
**	movprfx	z0, z1
**	mla	z0\.d, p0/m, z2\.d, z3\.d
** |
**	movprfx	z0, z2
**	mad	z0\.d, p0/m, z3\.d, z1\.d
** |
**	movprfx	z0, z3
**	mad	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (mla_u64_x_untied, svuint64_t,
		z0 = svmla_u64_x (p0, z1, z2, z3),
		z0 = svmla_x (p0, z1, z2, z3))

/*
** mla_x0_u64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	mla	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (mla_x0_u64_x_tied1, svuint64_t, uint64_t,
		 z0 = svmla_n_u64_x (p0, z0, z1, x0),
		 z0 = svmla_x (p0, z0, z1, x0))

/*
** mla_x0_u64_x_tied2:
**	mov	(z[0-9]+\.d), x0
**	mad	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (mla_x0_u64_x_tied2, svuint64_t, uint64_t,
		 z0 = svmla_n_u64_x (p0, z1, z0, x0),
		 z0 = svmla_x (p0, z1, z0, x0))

/*
** mla_x0_u64_x_untied:
**	mov	z0\.d, x0
**	mad	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (mla_x0_u64_x_untied, svuint64_t, uint64_t,
		 z0 = svmla_n_u64_x (p0, z1, z2, x0),
		 z0 = svmla_x (p0, z1, z2, x0))

/*
** mla_11_u64_x_tied1:
**	mov	(z[0-9]+\.d), #11
**	mla	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mla_11_u64_x_tied1, svuint64_t,
		z0 = svmla_n_u64_x (p0, z0, z1, 11),
		z0 = svmla_x (p0, z0, z1, 11))

/*
** mla_11_u64_x_tied2:
**	mov	(z[0-9]+\.d), #11
**	mad	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mla_11_u64_x_tied2, svuint64_t,
		z0 = svmla_n_u64_x (p0, z1, z0, 11),
		z0 = svmla_x (p0, z1, z0, 11))

/*
** mla_11_u64_x_untied:
**	mov	z0\.d, #11
**	mad	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mla_11_u64_x_untied, svuint64_t,
		z0 = svmla_n_u64_x (p0, z1, z2, 11),
		z0 = svmla_x (p0, z1, z2, 11))
