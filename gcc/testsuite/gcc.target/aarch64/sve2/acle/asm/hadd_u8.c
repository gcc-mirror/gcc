/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** hadd_u8_m_tied1:
**	uhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (hadd_u8_m_tied1, svuint8_t,
		z0 = svhadd_u8_m (p0, z0, z1),
		z0 = svhadd_m (p0, z0, z1))

/*
** hadd_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uhadd	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (hadd_u8_m_tied2, svuint8_t,
		z0 = svhadd_u8_m (p0, z1, z0),
		z0 = svhadd_m (p0, z1, z0))

/*
** hadd_u8_m_untied:
**	movprfx	z0, z1
**	uhadd	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (hadd_u8_m_untied, svuint8_t,
		z0 = svhadd_u8_m (p0, z1, z2),
		z0 = svhadd_m (p0, z1, z2))

/*
** hadd_w0_u8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	uhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (hadd_w0_u8_m_tied1, svuint8_t, uint8_t,
		 z0 = svhadd_n_u8_m (p0, z0, x0),
		 z0 = svhadd_m (p0, z0, x0))

/*
** hadd_w0_u8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	uhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (hadd_w0_u8_m_untied, svuint8_t, uint8_t,
		 z0 = svhadd_n_u8_m (p0, z1, x0),
		 z0 = svhadd_m (p0, z1, x0))

/*
** hadd_11_u8_m_tied1:
**	mov	(z[0-9]+\.b), #11
**	uhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (hadd_11_u8_m_tied1, svuint8_t,
		z0 = svhadd_n_u8_m (p0, z0, 11),
		z0 = svhadd_m (p0, z0, 11))

/*
** hadd_11_u8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0, z1
**	uhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (hadd_11_u8_m_untied, svuint8_t,
		z0 = svhadd_n_u8_m (p0, z1, 11),
		z0 = svhadd_m (p0, z1, 11))

/*
** hadd_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	uhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (hadd_u8_z_tied1, svuint8_t,
		z0 = svhadd_u8_z (p0, z0, z1),
		z0 = svhadd_z (p0, z0, z1))

/*
** hadd_u8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	uhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (hadd_u8_z_tied2, svuint8_t,
		z0 = svhadd_u8_z (p0, z1, z0),
		z0 = svhadd_z (p0, z1, z0))

/*
** hadd_u8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uhadd	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	uhadd	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (hadd_u8_z_untied, svuint8_t,
		z0 = svhadd_u8_z (p0, z1, z2),
		z0 = svhadd_z (p0, z1, z2))

/*
** hadd_w0_u8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	uhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (hadd_w0_u8_z_tied1, svuint8_t, uint8_t,
		 z0 = svhadd_n_u8_z (p0, z0, x0),
		 z0 = svhadd_z (p0, z0, x0))

/*
** hadd_w0_u8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uhadd	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	uhadd	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (hadd_w0_u8_z_untied, svuint8_t, uint8_t,
		 z0 = svhadd_n_u8_z (p0, z1, x0),
		 z0 = svhadd_z (p0, z1, x0))

/*
** hadd_11_u8_z_tied1:
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0\.b, p0/z, z0\.b
**	uhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (hadd_11_u8_z_tied1, svuint8_t,
		z0 = svhadd_n_u8_z (p0, z0, 11),
		z0 = svhadd_z (p0, z0, 11))

/*
** hadd_11_u8_z_untied:
**	mov	(z[0-9]+\.b), #11
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uhadd	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	uhadd	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (hadd_11_u8_z_untied, svuint8_t,
		z0 = svhadd_n_u8_z (p0, z1, 11),
		z0 = svhadd_z (p0, z1, 11))

/*
** hadd_u8_x_tied1:
**	uhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (hadd_u8_x_tied1, svuint8_t,
		z0 = svhadd_u8_x (p0, z0, z1),
		z0 = svhadd_x (p0, z0, z1))

/*
** hadd_u8_x_tied2:
**	uhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (hadd_u8_x_tied2, svuint8_t,
		z0 = svhadd_u8_x (p0, z1, z0),
		z0 = svhadd_x (p0, z1, z0))

/*
** hadd_u8_x_untied:
** (
**	movprfx	z0, z1
**	uhadd	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0, z2
**	uhadd	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (hadd_u8_x_untied, svuint8_t,
		z0 = svhadd_u8_x (p0, z1, z2),
		z0 = svhadd_x (p0, z1, z2))

/*
** hadd_w0_u8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	uhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (hadd_w0_u8_x_tied1, svuint8_t, uint8_t,
		 z0 = svhadd_n_u8_x (p0, z0, x0),
		 z0 = svhadd_x (p0, z0, x0))

/*
** hadd_w0_u8_x_untied:
**	mov	z0\.b, w0
**	uhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (hadd_w0_u8_x_untied, svuint8_t, uint8_t,
		 z0 = svhadd_n_u8_x (p0, z1, x0),
		 z0 = svhadd_x (p0, z1, x0))

/*
** hadd_11_u8_x_tied1:
**	mov	(z[0-9]+\.b), #11
**	uhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (hadd_11_u8_x_tied1, svuint8_t,
		z0 = svhadd_n_u8_x (p0, z0, 11),
		z0 = svhadd_x (p0, z0, 11))

/*
** hadd_11_u8_x_untied:
**	mov	z0\.b, #11
**	uhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (hadd_11_u8_x_untied, svuint8_t,
		z0 = svhadd_n_u8_x (p0, z1, 11),
		z0 = svhadd_x (p0, z1, 11))
