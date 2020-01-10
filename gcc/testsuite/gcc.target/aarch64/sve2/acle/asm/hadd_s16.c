/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** hadd_s16_m_tied1:
**	shadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (hadd_s16_m_tied1, svint16_t,
		z0 = svhadd_s16_m (p0, z0, z1),
		z0 = svhadd_m (p0, z0, z1))

/*
** hadd_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	shadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (hadd_s16_m_tied2, svint16_t,
		z0 = svhadd_s16_m (p0, z1, z0),
		z0 = svhadd_m (p0, z1, z0))

/*
** hadd_s16_m_untied:
**	movprfx	z0, z1
**	shadd	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (hadd_s16_m_untied, svint16_t,
		z0 = svhadd_s16_m (p0, z1, z2),
		z0 = svhadd_m (p0, z1, z2))

/*
** hadd_w0_s16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	shadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (hadd_w0_s16_m_tied1, svint16_t, int16_t,
		 z0 = svhadd_n_s16_m (p0, z0, x0),
		 z0 = svhadd_m (p0, z0, x0))

/*
** hadd_w0_s16_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	shadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (hadd_w0_s16_m_untied, svint16_t, int16_t,
		 z0 = svhadd_n_s16_m (p0, z1, x0),
		 z0 = svhadd_m (p0, z1, x0))

/*
** hadd_11_s16_m_tied1:
**	mov	(z[0-9]+\.h), #11
**	shadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (hadd_11_s16_m_tied1, svint16_t,
		z0 = svhadd_n_s16_m (p0, z0, 11),
		z0 = svhadd_m (p0, z0, 11))

/*
** hadd_11_s16_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0, z1
**	shadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (hadd_11_s16_m_untied, svint16_t,
		z0 = svhadd_n_s16_m (p0, z1, 11),
		z0 = svhadd_m (p0, z1, 11))

/*
** hadd_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	shadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (hadd_s16_z_tied1, svint16_t,
		z0 = svhadd_s16_z (p0, z0, z1),
		z0 = svhadd_z (p0, z0, z1))

/*
** hadd_s16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	shadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (hadd_s16_z_tied2, svint16_t,
		z0 = svhadd_s16_z (p0, z1, z0),
		z0 = svhadd_z (p0, z1, z0))

/*
** hadd_s16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	shadd	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	shadd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (hadd_s16_z_untied, svint16_t,
		z0 = svhadd_s16_z (p0, z1, z2),
		z0 = svhadd_z (p0, z1, z2))

/*
** hadd_w0_s16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	shadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (hadd_w0_s16_z_tied1, svint16_t, int16_t,
		 z0 = svhadd_n_s16_z (p0, z0, x0),
		 z0 = svhadd_z (p0, z0, x0))

/*
** hadd_w0_s16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	shadd	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	shadd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (hadd_w0_s16_z_untied, svint16_t, int16_t,
		 z0 = svhadd_n_s16_z (p0, z1, x0),
		 z0 = svhadd_z (p0, z1, x0))

/*
** hadd_11_s16_z_tied1:
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0\.h, p0/z, z0\.h
**	shadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (hadd_11_s16_z_tied1, svint16_t,
		z0 = svhadd_n_s16_z (p0, z0, 11),
		z0 = svhadd_z (p0, z0, 11))

/*
** hadd_11_s16_z_untied:
**	mov	(z[0-9]+\.h), #11
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	shadd	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	shadd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (hadd_11_s16_z_untied, svint16_t,
		z0 = svhadd_n_s16_z (p0, z1, 11),
		z0 = svhadd_z (p0, z1, 11))

/*
** hadd_s16_x_tied1:
**	shadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (hadd_s16_x_tied1, svint16_t,
		z0 = svhadd_s16_x (p0, z0, z1),
		z0 = svhadd_x (p0, z0, z1))

/*
** hadd_s16_x_tied2:
**	shadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (hadd_s16_x_tied2, svint16_t,
		z0 = svhadd_s16_x (p0, z1, z0),
		z0 = svhadd_x (p0, z1, z0))

/*
** hadd_s16_x_untied:
** (
**	movprfx	z0, z1
**	shadd	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0, z2
**	shadd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (hadd_s16_x_untied, svint16_t,
		z0 = svhadd_s16_x (p0, z1, z2),
		z0 = svhadd_x (p0, z1, z2))

/*
** hadd_w0_s16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	shadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (hadd_w0_s16_x_tied1, svint16_t, int16_t,
		 z0 = svhadd_n_s16_x (p0, z0, x0),
		 z0 = svhadd_x (p0, z0, x0))

/*
** hadd_w0_s16_x_untied:
**	mov	z0\.h, w0
**	shadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (hadd_w0_s16_x_untied, svint16_t, int16_t,
		 z0 = svhadd_n_s16_x (p0, z1, x0),
		 z0 = svhadd_x (p0, z1, x0))

/*
** hadd_11_s16_x_tied1:
**	mov	(z[0-9]+\.h), #11
**	shadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (hadd_11_s16_x_tied1, svint16_t,
		z0 = svhadd_n_s16_x (p0, z0, 11),
		z0 = svhadd_x (p0, z0, 11))

/*
** hadd_11_s16_x_untied:
**	mov	z0\.h, #11
**	shadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (hadd_11_s16_x_untied, svint16_t,
		z0 = svhadd_n_s16_x (p0, z1, 11),
		z0 = svhadd_x (p0, z1, 11))
