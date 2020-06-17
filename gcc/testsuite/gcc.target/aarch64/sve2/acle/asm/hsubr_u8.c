/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** hsubr_u8_m_tied1:
**	uhsubr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (hsubr_u8_m_tied1, svuint8_t,
		z0 = svhsubr_u8_m (p0, z0, z1),
		z0 = svhsubr_m (p0, z0, z1))

/*
** hsubr_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uhsubr	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (hsubr_u8_m_tied2, svuint8_t,
		z0 = svhsubr_u8_m (p0, z1, z0),
		z0 = svhsubr_m (p0, z1, z0))

/*
** hsubr_u8_m_untied:
**	movprfx	z0, z1
**	uhsubr	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (hsubr_u8_m_untied, svuint8_t,
		z0 = svhsubr_u8_m (p0, z1, z2),
		z0 = svhsubr_m (p0, z1, z2))

/*
** hsubr_w0_u8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	uhsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (hsubr_w0_u8_m_tied1, svuint8_t, uint8_t,
		 z0 = svhsubr_n_u8_m (p0, z0, x0),
		 z0 = svhsubr_m (p0, z0, x0))

/*
** hsubr_w0_u8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	uhsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (hsubr_w0_u8_m_untied, svuint8_t, uint8_t,
		 z0 = svhsubr_n_u8_m (p0, z1, x0),
		 z0 = svhsubr_m (p0, z1, x0))

/*
** hsubr_11_u8_m_tied1:
**	mov	(z[0-9]+\.b), #11
**	uhsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (hsubr_11_u8_m_tied1, svuint8_t,
		z0 = svhsubr_n_u8_m (p0, z0, 11),
		z0 = svhsubr_m (p0, z0, 11))

/*
** hsubr_11_u8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0, z1
**	uhsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (hsubr_11_u8_m_untied, svuint8_t,
		z0 = svhsubr_n_u8_m (p0, z1, 11),
		z0 = svhsubr_m (p0, z1, 11))

/*
** hsubr_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	uhsubr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (hsubr_u8_z_tied1, svuint8_t,
		z0 = svhsubr_u8_z (p0, z0, z1),
		z0 = svhsubr_z (p0, z0, z1))

/*
** hsubr_u8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	uhsub	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (hsubr_u8_z_tied2, svuint8_t,
		z0 = svhsubr_u8_z (p0, z1, z0),
		z0 = svhsubr_z (p0, z1, z0))

/*
** hsubr_u8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uhsubr	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	uhsub	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (hsubr_u8_z_untied, svuint8_t,
		z0 = svhsubr_u8_z (p0, z1, z2),
		z0 = svhsubr_z (p0, z1, z2))

/*
** hsubr_w0_u8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	uhsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (hsubr_w0_u8_z_tied1, svuint8_t, uint8_t,
		 z0 = svhsubr_n_u8_z (p0, z0, x0),
		 z0 = svhsubr_z (p0, z0, x0))

/*
** hsubr_w0_u8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uhsubr	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	uhsub	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (hsubr_w0_u8_z_untied, svuint8_t, uint8_t,
		 z0 = svhsubr_n_u8_z (p0, z1, x0),
		 z0 = svhsubr_z (p0, z1, x0))

/*
** hsubr_11_u8_z_tied1:
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0\.b, p0/z, z0\.b
**	uhsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (hsubr_11_u8_z_tied1, svuint8_t,
		z0 = svhsubr_n_u8_z (p0, z0, 11),
		z0 = svhsubr_z (p0, z0, 11))

/*
** hsubr_11_u8_z_untied:
**	mov	(z[0-9]+\.b), #11
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uhsubr	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	uhsub	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (hsubr_11_u8_z_untied, svuint8_t,
		z0 = svhsubr_n_u8_z (p0, z1, 11),
		z0 = svhsubr_z (p0, z1, 11))

/*
** hsubr_u8_x_tied1:
**	uhsubr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (hsubr_u8_x_tied1, svuint8_t,
		z0 = svhsubr_u8_x (p0, z0, z1),
		z0 = svhsubr_x (p0, z0, z1))

/*
** hsubr_u8_x_tied2:
**	uhsub	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (hsubr_u8_x_tied2, svuint8_t,
		z0 = svhsubr_u8_x (p0, z1, z0),
		z0 = svhsubr_x (p0, z1, z0))

/*
** hsubr_u8_x_untied:
** (
**	movprfx	z0, z1
**	uhsubr	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0, z2
**	uhsub	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (hsubr_u8_x_untied, svuint8_t,
		z0 = svhsubr_u8_x (p0, z1, z2),
		z0 = svhsubr_x (p0, z1, z2))

/*
** hsubr_w0_u8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	uhsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (hsubr_w0_u8_x_tied1, svuint8_t, uint8_t,
		 z0 = svhsubr_n_u8_x (p0, z0, x0),
		 z0 = svhsubr_x (p0, z0, x0))

/*
** hsubr_w0_u8_x_untied:
**	mov	z0\.b, w0
**	uhsub	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (hsubr_w0_u8_x_untied, svuint8_t, uint8_t,
		 z0 = svhsubr_n_u8_x (p0, z1, x0),
		 z0 = svhsubr_x (p0, z1, x0))

/*
** hsubr_11_u8_x_tied1:
**	mov	(z[0-9]+\.b), #11
**	uhsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (hsubr_11_u8_x_tied1, svuint8_t,
		z0 = svhsubr_n_u8_x (p0, z0, 11),
		z0 = svhsubr_x (p0, z0, 11))

/*
** hsubr_11_u8_x_untied:
**	mov	z0\.b, #11
**	uhsub	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (hsubr_11_u8_x_untied, svuint8_t,
		z0 = svhsubr_n_u8_x (p0, z1, 11),
		z0 = svhsubr_x (p0, z1, 11))
