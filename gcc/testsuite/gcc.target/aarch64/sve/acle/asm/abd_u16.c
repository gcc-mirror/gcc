/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abd_u16_m_tied1:
**	uabd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (abd_u16_m_tied1, svuint16_t,
		z0 = svabd_u16_m (p0, z0, z1),
		z0 = svabd_m (p0, z0, z1))

/*
** abd_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uabd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (abd_u16_m_tied2, svuint16_t,
		z0 = svabd_u16_m (p0, z1, z0),
		z0 = svabd_m (p0, z1, z0))

/*
** abd_u16_m_untied:
**	movprfx	z0, z1
**	uabd	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (abd_u16_m_untied, svuint16_t,
		z0 = svabd_u16_m (p0, z1, z2),
		z0 = svabd_m (p0, z1, z2))

/*
** abd_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	uabd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (abd_w0_u16_m_tied1, svuint16_t, uint16_t,
		 z0 = svabd_n_u16_m (p0, z0, x0),
		 z0 = svabd_m (p0, z0, x0))

/*
** abd_w0_u16_m_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	uabd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (abd_w0_u16_m_untied, svuint16_t, uint16_t,
		 z0 = svabd_n_u16_m (p0, z1, x0),
		 z0 = svabd_m (p0, z1, x0))

/*
** abd_1_u16_m_tied1:
**	mov	(z[0-9]+\.h), #1
**	uabd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_u16_m_tied1, svuint16_t,
		z0 = svabd_n_u16_m (p0, z0, 1),
		z0 = svabd_m (p0, z0, 1))

/*
** abd_1_u16_m_untied:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0, z1
**	uabd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_u16_m_untied, svuint16_t,
		z0 = svabd_n_u16_m (p0, z1, 1),
		z0 = svabd_m (p0, z1, 1))

/*
** abd_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	uabd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (abd_u16_z_tied1, svuint16_t,
		z0 = svabd_u16_z (p0, z0, z1),
		z0 = svabd_z (p0, z0, z1))

/*
** abd_u16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	uabd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (abd_u16_z_tied2, svuint16_t,
		z0 = svabd_u16_z (p0, z1, z0),
		z0 = svabd_z (p0, z1, z0))

/*
** abd_u16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	uabd	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	uabd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (abd_u16_z_untied, svuint16_t,
		z0 = svabd_u16_z (p0, z1, z2),
		z0 = svabd_z (p0, z1, z2))

/*
** abd_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	uabd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (abd_w0_u16_z_tied1, svuint16_t, uint16_t,
		 z0 = svabd_n_u16_z (p0, z0, x0),
		 z0 = svabd_z (p0, z0, x0))

/*
** abd_w0_u16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	uabd	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	uabd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (abd_w0_u16_z_untied, svuint16_t, uint16_t,
		 z0 = svabd_n_u16_z (p0, z1, x0),
		 z0 = svabd_z (p0, z1, x0))

/*
** abd_1_u16_z_tied1:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z0\.h
**	uabd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_u16_z_tied1, svuint16_t,
		z0 = svabd_n_u16_z (p0, z0, 1),
		z0 = svabd_z (p0, z0, 1))

/*
** abd_1_u16_z_untied:
**	mov	(z[0-9]+\.h), #1
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	uabd	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	uabd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (abd_1_u16_z_untied, svuint16_t,
		z0 = svabd_n_u16_z (p0, z1, 1),
		z0 = svabd_z (p0, z1, 1))

/*
** abd_u16_x_tied1:
**	uabd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (abd_u16_x_tied1, svuint16_t,
		z0 = svabd_u16_x (p0, z0, z1),
		z0 = svabd_x (p0, z0, z1))

/*
** abd_u16_x_tied2:
**	uabd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (abd_u16_x_tied2, svuint16_t,
		z0 = svabd_u16_x (p0, z1, z0),
		z0 = svabd_x (p0, z1, z0))

/*
** abd_u16_x_untied:
** (
**	movprfx	z0, z1
**	uabd	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0, z2
**	uabd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (abd_u16_x_untied, svuint16_t,
		z0 = svabd_u16_x (p0, z1, z2),
		z0 = svabd_x (p0, z1, z2))

/*
** abd_w0_u16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	uabd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (abd_w0_u16_x_tied1, svuint16_t, uint16_t,
		 z0 = svabd_n_u16_x (p0, z0, x0),
		 z0 = svabd_x (p0, z0, x0))

/*
** abd_w0_u16_x_untied:
**	mov	z0\.h, w0
**	uabd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (abd_w0_u16_x_untied, svuint16_t, uint16_t,
		 z0 = svabd_n_u16_x (p0, z1, x0),
		 z0 = svabd_x (p0, z1, x0))

/*
** abd_1_u16_x_tied1:
**	mov	(z[0-9]+\.h), #1
**	uabd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_u16_x_tied1, svuint16_t,
		z0 = svabd_n_u16_x (p0, z0, 1),
		z0 = svabd_x (p0, z0, 1))

/*
** abd_1_u16_x_untied:
**	mov	z0\.h, #1
**	uabd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (abd_1_u16_x_untied, svuint16_t,
		z0 = svabd_n_u16_x (p0, z1, 1),
		z0 = svabd_x (p0, z1, 1))
