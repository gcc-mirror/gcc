/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** msb_u64_m_tied1:
**	msb	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (msb_u64_m_tied1, svuint64_t,
		z0 = svmsb_u64_m (p0, z0, z1, z2),
		z0 = svmsb_m (p0, z0, z1, z2))

/*
** msb_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	msb	z0\.d, p0/m, \1, z2\.d
**	ret
*/
TEST_UNIFORM_Z (msb_u64_m_tied2, svuint64_t,
		z0 = svmsb_u64_m (p0, z1, z0, z2),
		z0 = svmsb_m (p0, z1, z0, z2))

/*
** msb_u64_m_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	msb	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (msb_u64_m_tied3, svuint64_t,
		z0 = svmsb_u64_m (p0, z1, z2, z0),
		z0 = svmsb_m (p0, z1, z2, z0))

/*
** msb_u64_m_untied:
**	movprfx	z0, z1
**	msb	z0\.d, p0/m, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (msb_u64_m_untied, svuint64_t,
		z0 = svmsb_u64_m (p0, z1, z2, z3),
		z0 = svmsb_m (p0, z1, z2, z3))

/*
** msb_x0_u64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	msb	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (msb_x0_u64_m_tied1, svuint64_t, uint64_t,
		 z0 = svmsb_n_u64_m (p0, z0, z1, x0),
		 z0 = svmsb_m (p0, z0, z1, x0))

/*
** msb_x0_u64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	msb	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (msb_x0_u64_m_untied, svuint64_t, uint64_t,
		 z0 = svmsb_n_u64_m (p0, z1, z2, x0),
		 z0 = svmsb_m (p0, z1, z2, x0))

/*
** msb_11_u64_m_tied1:
**	mov	(z[0-9]+\.d), #11
**	msb	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (msb_11_u64_m_tied1, svuint64_t,
		z0 = svmsb_n_u64_m (p0, z0, z1, 11),
		z0 = svmsb_m (p0, z0, z1, 11))

/*
** msb_11_u64_m_untied:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0, z1
**	msb	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (msb_11_u64_m_untied, svuint64_t,
		z0 = svmsb_n_u64_m (p0, z1, z2, 11),
		z0 = svmsb_m (p0, z1, z2, 11))

/*
** msb_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	msb	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (msb_u64_z_tied1, svuint64_t,
		z0 = svmsb_u64_z (p0, z0, z1, z2),
		z0 = svmsb_z (p0, z0, z1, z2))

/*
** msb_u64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	msb	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (msb_u64_z_tied2, svuint64_t,
		z0 = svmsb_u64_z (p0, z1, z0, z2),
		z0 = svmsb_z (p0, z1, z0, z2))

/*
** msb_u64_z_tied3:
**	movprfx	z0\.d, p0/z, z0\.d
**	mls	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (msb_u64_z_tied3, svuint64_t,
		z0 = svmsb_u64_z (p0, z1, z2, z0),
		z0 = svmsb_z (p0, z1, z2, z0))

/*
** msb_u64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	msb	z0\.d, p0/m, z2\.d, z3\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	msb	z0\.d, p0/m, z1\.d, z3\.d
** |
**	movprfx	z0\.d, p0/z, z3\.d
**	mls	z0\.d, p0/m, z1\.d, z2\.d
** )
**	ret
*/
TEST_UNIFORM_Z (msb_u64_z_untied, svuint64_t,
		z0 = svmsb_u64_z (p0, z1, z2, z3),
		z0 = svmsb_z (p0, z1, z2, z3))

/*
** msb_x0_u64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	msb	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (msb_x0_u64_z_tied1, svuint64_t, uint64_t,
		 z0 = svmsb_n_u64_z (p0, z0, z1, x0),
		 z0 = svmsb_z (p0, z0, z1, x0))

/*
** msb_x0_u64_z_tied2:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	msb	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (msb_x0_u64_z_tied2, svuint64_t, uint64_t,
		 z0 = svmsb_n_u64_z (p0, z1, z0, x0),
		 z0 = svmsb_z (p0, z1, z0, x0))

/*
** msb_x0_u64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	msb	z0\.d, p0/m, z2\.d, \1
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	msb	z0\.d, p0/m, z1\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	mls	z0\.d, p0/m, z1\.d, z2\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (msb_x0_u64_z_untied, svuint64_t, uint64_t,
		 z0 = svmsb_n_u64_z (p0, z1, z2, x0),
		 z0 = svmsb_z (p0, z1, z2, x0))

/*
** msb_11_u64_z_tied1:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0\.d, p0/z, z0\.d
**	msb	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (msb_11_u64_z_tied1, svuint64_t,
		z0 = svmsb_n_u64_z (p0, z0, z1, 11),
		z0 = svmsb_z (p0, z0, z1, 11))

/*
** msb_11_u64_z_tied2:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0\.d, p0/z, z0\.d
**	msb	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (msb_11_u64_z_tied2, svuint64_t,
		z0 = svmsb_n_u64_z (p0, z1, z0, 11),
		z0 = svmsb_z (p0, z1, z0, 11))

/*
** msb_11_u64_z_untied:
**	mov	(z[0-9]+\.d), #11
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	msb	z0\.d, p0/m, z2\.d, \1
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	msb	z0\.d, p0/m, z1\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	mls	z0\.d, p0/m, z1\.d, z2\.d
** )
**	ret
*/
TEST_UNIFORM_Z (msb_11_u64_z_untied, svuint64_t,
		z0 = svmsb_n_u64_z (p0, z1, z2, 11),
		z0 = svmsb_z (p0, z1, z2, 11))

/*
** msb_u64_x_tied1:
**	msb	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (msb_u64_x_tied1, svuint64_t,
		z0 = svmsb_u64_x (p0, z0, z1, z2),
		z0 = svmsb_x (p0, z0, z1, z2))

/*
** msb_u64_x_tied2:
**	msb	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (msb_u64_x_tied2, svuint64_t,
		z0 = svmsb_u64_x (p0, z1, z0, z2),
		z0 = svmsb_x (p0, z1, z0, z2))

/*
** msb_u64_x_tied3:
**	mls	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (msb_u64_x_tied3, svuint64_t,
		z0 = svmsb_u64_x (p0, z1, z2, z0),
		z0 = svmsb_x (p0, z1, z2, z0))

/*
** msb_u64_x_untied:
** (
**	movprfx	z0, z1
**	msb	z0\.d, p0/m, z2\.d, z3\.d
** |
**	movprfx	z0, z2
**	msb	z0\.d, p0/m, z1\.d, z3\.d
** |
**	movprfx	z0, z3
**	mls	z0\.d, p0/m, z1\.d, z2\.d
** )
**	ret
*/
TEST_UNIFORM_Z (msb_u64_x_untied, svuint64_t,
		z0 = svmsb_u64_x (p0, z1, z2, z3),
		z0 = svmsb_x (p0, z1, z2, z3))

/*
** msb_x0_u64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	msb	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (msb_x0_u64_x_tied1, svuint64_t, uint64_t,
		 z0 = svmsb_n_u64_x (p0, z0, z1, x0),
		 z0 = svmsb_x (p0, z0, z1, x0))

/*
** msb_x0_u64_x_tied2:
**	mov	(z[0-9]+\.d), x0
**	msb	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (msb_x0_u64_x_tied2, svuint64_t, uint64_t,
		 z0 = svmsb_n_u64_x (p0, z1, z0, x0),
		 z0 = svmsb_x (p0, z1, z0, x0))

/*
** msb_x0_u64_x_untied:
**	mov	z0\.d, x0
**	mls	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_ZX (msb_x0_u64_x_untied, svuint64_t, uint64_t,
		 z0 = svmsb_n_u64_x (p0, z1, z2, x0),
		 z0 = svmsb_x (p0, z1, z2, x0))

/*
** msb_11_u64_x_tied1:
**	mov	(z[0-9]+\.d), #11
**	msb	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (msb_11_u64_x_tied1, svuint64_t,
		z0 = svmsb_n_u64_x (p0, z0, z1, 11),
		z0 = svmsb_x (p0, z0, z1, 11))

/*
** msb_11_u64_x_tied2:
**	mov	(z[0-9]+\.d), #11
**	msb	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (msb_11_u64_x_tied2, svuint64_t,
		z0 = svmsb_n_u64_x (p0, z1, z0, 11),
		z0 = svmsb_x (p0, z1, z0, 11))

/*
** msb_11_u64_x_untied:
**	mov	z0\.d, #11
**	mls	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (msb_11_u64_x_untied, svuint64_t,
		z0 = svmsb_n_u64_x (p0, z1, z2, 11),
		z0 = svmsb_x (p0, z1, z2, 11))
