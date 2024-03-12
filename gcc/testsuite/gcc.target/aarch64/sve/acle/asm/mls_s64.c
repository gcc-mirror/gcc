/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mls_s64_m_tied1:
**	mls	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mls_s64_m_tied1, svint64_t,
		z0 = svmls_s64_m (p0, z0, z1, z2),
		z0 = svmls_m (p0, z0, z1, z2))

/*
** mls_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	mls	z0\.d, p0/m, \1, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mls_s64_m_tied2, svint64_t,
		z0 = svmls_s64_m (p0, z1, z0, z2),
		z0 = svmls_m (p0, z1, z0, z2))

/*
** mls_s64_m_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	mls	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mls_s64_m_tied3, svint64_t,
		z0 = svmls_s64_m (p0, z1, z2, z0),
		z0 = svmls_m (p0, z1, z2, z0))

/*
** mls_s64_m_untied:
**	movprfx	z0, z1
**	mls	z0\.d, p0/m, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (mls_s64_m_untied, svint64_t,
		z0 = svmls_s64_m (p0, z1, z2, z3),
		z0 = svmls_m (p0, z1, z2, z3))

/*
** mls_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	mls	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (mls_x0_s64_m_tied1, svint64_t, int64_t,
		 z0 = svmls_n_s64_m (p0, z0, z1, x0),
		 z0 = svmls_m (p0, z0, z1, x0))

/*
** mls_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	mls	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (mls_x0_s64_m_untied, svint64_t, int64_t,
		 z0 = svmls_n_s64_m (p0, z1, z2, x0),
		 z0 = svmls_m (p0, z1, z2, x0))

/*
** mls_11_s64_m_tied1:
**	mov	(z[0-9]+\.d), #11
**	mls	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mls_11_s64_m_tied1, svint64_t,
		z0 = svmls_n_s64_m (p0, z0, z1, 11),
		z0 = svmls_m (p0, z0, z1, 11))

/*
** mls_11_s64_m_untied:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0, z1
**	mls	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mls_11_s64_m_untied, svint64_t,
		z0 = svmls_n_s64_m (p0, z1, z2, 11),
		z0 = svmls_m (p0, z1, z2, 11))

/*
** mls_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	mls	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mls_s64_z_tied1, svint64_t,
		z0 = svmls_s64_z (p0, z0, z1, z2),
		z0 = svmls_z (p0, z0, z1, z2))

/*
** mls_s64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	msb	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_s64_z_tied2, svint64_t,
		z0 = svmls_s64_z (p0, z1, z0, z2),
		z0 = svmls_z (p0, z1, z0, z2))

/*
** mls_s64_z_tied3:
**	movprfx	z0\.d, p0/z, z0\.d
**	msb	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_s64_z_tied3, svint64_t,
		z0 = svmls_s64_z (p0, z1, z2, z0),
		z0 = svmls_z (p0, z1, z2, z0))

/*
** mls_s64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	mls	z0\.d, p0/m, z2\.d, z3\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	msb	z0\.d, p0/m, z3\.d, z1\.d
** |
**	movprfx	z0\.d, p0/z, z3\.d
**	msb	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (mls_s64_z_untied, svint64_t,
		z0 = svmls_s64_z (p0, z1, z2, z3),
		z0 = svmls_z (p0, z1, z2, z3))

/*
** mls_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	mls	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (mls_x0_s64_z_tied1, svint64_t, int64_t,
		 z0 = svmls_n_s64_z (p0, z0, z1, x0),
		 z0 = svmls_z (p0, z0, z1, x0))

/*
** mls_x0_s64_z_tied2:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	msb	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (mls_x0_s64_z_tied2, svint64_t, int64_t,
		 z0 = svmls_n_s64_z (p0, z1, z0, x0),
		 z0 = svmls_z (p0, z1, z0, x0))

/*
** mls_x0_s64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	mls	z0\.d, p0/m, z2\.d, \1
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	msb	z0\.d, p0/m, \1, z1\.d
** |
**	movprfx	z0\.d, p0/z, \1
**	msb	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (mls_x0_s64_z_untied, svint64_t, int64_t,
		 z0 = svmls_n_s64_z (p0, z1, z2, x0),
		 z0 = svmls_z (p0, z1, z2, x0))

/*
** mls_11_s64_z_tied1:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0\.d, p0/z, z0\.d
**	mls	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mls_11_s64_z_tied1, svint64_t,
		z0 = svmls_n_s64_z (p0, z0, z1, 11),
		z0 = svmls_z (p0, z0, z1, 11))

/*
** mls_11_s64_z_tied2:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0\.d, p0/z, z0\.d
**	msb	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_11_s64_z_tied2, svint64_t,
		z0 = svmls_n_s64_z (p0, z1, z0, 11),
		z0 = svmls_z (p0, z1, z0, 11))

/*
** mls_11_s64_z_untied:
**	mov	(z[0-9]+\.d), #11
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	mls	z0\.d, p0/m, z2\.d, \1
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	msb	z0\.d, p0/m, \1, z1\.d
** |
**	movprfx	z0\.d, p0/z, \1
**	msb	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (mls_11_s64_z_untied, svint64_t,
		z0 = svmls_n_s64_z (p0, z1, z2, 11),
		z0 = svmls_z (p0, z1, z2, 11))

/*
** mls_s64_x_tied1:
**	mls	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mls_s64_x_tied1, svint64_t,
		z0 = svmls_s64_x (p0, z0, z1, z2),
		z0 = svmls_x (p0, z0, z1, z2))

/*
** mls_s64_x_tied2:
**	msb	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_s64_x_tied2, svint64_t,
		z0 = svmls_s64_x (p0, z1, z0, z2),
		z0 = svmls_x (p0, z1, z0, z2))

/*
** mls_s64_x_tied3:
**	msb	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_s64_x_tied3, svint64_t,
		z0 = svmls_s64_x (p0, z1, z2, z0),
		z0 = svmls_x (p0, z1, z2, z0))

/*
** mls_s64_x_untied:
** (
**	movprfx	z0, z1
**	mls	z0\.d, p0/m, z2\.d, z3\.d
** |
**	movprfx	z0, z2
**	msb	z0\.d, p0/m, z3\.d, z1\.d
** |
**	movprfx	z0, z3
**	msb	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (mls_s64_x_untied, svint64_t,
		z0 = svmls_s64_x (p0, z1, z2, z3),
		z0 = svmls_x (p0, z1, z2, z3))

/*
** mls_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	mls	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (mls_x0_s64_x_tied1, svint64_t, int64_t,
		 z0 = svmls_n_s64_x (p0, z0, z1, x0),
		 z0 = svmls_x (p0, z0, z1, x0))

/*
** mls_x0_s64_x_tied2:
**	mov	(z[0-9]+\.d), x0
**	msb	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (mls_x0_s64_x_tied2, svint64_t, int64_t,
		 z0 = svmls_n_s64_x (p0, z1, z0, x0),
		 z0 = svmls_x (p0, z1, z0, x0))

/*
** mls_x0_s64_x_untied:
**	mov	z0\.d, x0
**	msb	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (mls_x0_s64_x_untied, svint64_t, int64_t,
		 z0 = svmls_n_s64_x (p0, z1, z2, x0),
		 z0 = svmls_x (p0, z1, z2, x0))

/*
** mls_11_s64_x_tied1:
**	mov	(z[0-9]+\.d), #11
**	mls	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mls_11_s64_x_tied1, svint64_t,
		z0 = svmls_n_s64_x (p0, z0, z1, 11),
		z0 = svmls_x (p0, z0, z1, 11))

/*
** mls_11_s64_x_tied2:
**	mov	(z[0-9]+\.d), #11
**	msb	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_11_s64_x_tied2, svint64_t,
		z0 = svmls_n_s64_x (p0, z1, z0, 11),
		z0 = svmls_x (p0, z1, z0, 11))

/*
** mls_11_s64_x_untied:
**	mov	z0\.d, #11
**	msb	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_11_s64_x_untied, svint64_t,
		z0 = svmls_n_s64_x (p0, z1, z2, 11),
		z0 = svmls_x (p0, z1, z2, 11))
