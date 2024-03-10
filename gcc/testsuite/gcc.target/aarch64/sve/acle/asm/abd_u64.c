/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abd_u64_m_tied1:
**	uabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_u64_m_tied1, svuint64_t,
		z0 = svabd_u64_m (p0, z0, z1),
		z0 = svabd_m (p0, z0, z1))

/*
** abd_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	uabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_u64_m_tied2, svuint64_t,
		z0 = svabd_u64_m (p0, z1, z0),
		z0 = svabd_m (p0, z1, z0))

/*
** abd_u64_m_untied:
**	movprfx	z0, z1
**	uabd	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (abd_u64_m_untied, svuint64_t,
		z0 = svabd_u64_m (p0, z1, z2),
		z0 = svabd_m (p0, z1, z2))

/*
** abd_x0_u64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	uabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (abd_x0_u64_m_tied1, svuint64_t, uint64_t,
		 z0 = svabd_n_u64_m (p0, z0, x0),
		 z0 = svabd_m (p0, z0, x0))

/*
** abd_x0_u64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	uabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (abd_x0_u64_m_untied, svuint64_t, uint64_t,
		 z0 = svabd_n_u64_m (p0, z1, x0),
		 z0 = svabd_m (p0, z1, x0))

/*
** abd_1_u64_m_tied1:
**	mov	(z[0-9]+\.d), #1
**	uabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_u64_m_tied1, svuint64_t,
		z0 = svabd_n_u64_m (p0, z0, 1),
		z0 = svabd_m (p0, z0, 1))

/*
** abd_1_u64_m_untied:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0, z1
**	uabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_u64_m_untied, svuint64_t,
		z0 = svabd_n_u64_m (p0, z1, 1),
		z0 = svabd_m (p0, z1, 1))

/*
** abd_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	uabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_u64_z_tied1, svuint64_t,
		z0 = svabd_u64_z (p0, z0, z1),
		z0 = svabd_z (p0, z0, z1))

/*
** abd_u64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	uabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_u64_z_tied2, svuint64_t,
		z0 = svabd_u64_z (p0, z1, z0),
		z0 = svabd_z (p0, z1, z0))

/*
** abd_u64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	uabd	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	uabd	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (abd_u64_z_untied, svuint64_t,
		z0 = svabd_u64_z (p0, z1, z2),
		z0 = svabd_z (p0, z1, z2))

/*
** abd_x0_u64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	uabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (abd_x0_u64_z_tied1, svuint64_t, uint64_t,
		 z0 = svabd_n_u64_z (p0, z0, x0),
		 z0 = svabd_z (p0, z0, x0))

/*
** abd_x0_u64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	uabd	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	uabd	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (abd_x0_u64_z_untied, svuint64_t, uint64_t,
		 z0 = svabd_n_u64_z (p0, z1, x0),
		 z0 = svabd_z (p0, z1, x0))

/*
** abd_1_u64_z_tied1:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0\.d, p0/z, z0\.d
**	uabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_u64_z_tied1, svuint64_t,
		z0 = svabd_n_u64_z (p0, z0, 1),
		z0 = svabd_z (p0, z0, 1))

/*
** abd_1_u64_z_untied:
**	mov	(z[0-9]+\.d), #1
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	uabd	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	uabd	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (abd_1_u64_z_untied, svuint64_t,
		z0 = svabd_n_u64_z (p0, z1, 1),
		z0 = svabd_z (p0, z1, 1))

/*
** abd_u64_x_tied1:
**	uabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_u64_x_tied1, svuint64_t,
		z0 = svabd_u64_x (p0, z0, z1),
		z0 = svabd_x (p0, z0, z1))

/*
** abd_u64_x_tied2:
**	uabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_u64_x_tied2, svuint64_t,
		z0 = svabd_u64_x (p0, z1, z0),
		z0 = svabd_x (p0, z1, z0))

/*
** abd_u64_x_untied:
** (
**	movprfx	z0, z1
**	uabd	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0, z2
**	uabd	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (abd_u64_x_untied, svuint64_t,
		z0 = svabd_u64_x (p0, z1, z2),
		z0 = svabd_x (p0, z1, z2))

/*
** abd_x0_u64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	uabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (abd_x0_u64_x_tied1, svuint64_t, uint64_t,
		 z0 = svabd_n_u64_x (p0, z0, x0),
		 z0 = svabd_x (p0, z0, x0))

/*
** abd_x0_u64_x_untied:
**	mov	z0\.d, x0
**	uabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (abd_x0_u64_x_untied, svuint64_t, uint64_t,
		 z0 = svabd_n_u64_x (p0, z1, x0),
		 z0 = svabd_x (p0, z1, x0))

/*
** abd_1_u64_x_tied1:
**	mov	(z[0-9]+\.d), #1
**	uabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_u64_x_tied1, svuint64_t,
		z0 = svabd_n_u64_x (p0, z0, 1),
		z0 = svabd_x (p0, z0, 1))

/*
** abd_1_u64_x_untied:
**	mov	z0\.d, #1
**	uabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_1_u64_x_untied, svuint64_t,
		z0 = svabd_n_u64_x (p0, z1, 1),
		z0 = svabd_x (p0, z1, 1))
