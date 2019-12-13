/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mulh_u16_m_tied1:
**	umulh	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mulh_u16_m_tied1, svuint16_t,
		z0 = svmulh_u16_m (p0, z0, z1),
		z0 = svmulh_m (p0, z0, z1))

/*
** mulh_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	umulh	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (mulh_u16_m_tied2, svuint16_t,
		z0 = svmulh_u16_m (p0, z1, z0),
		z0 = svmulh_m (p0, z1, z0))

/*
** mulh_u16_m_untied:
**	movprfx	z0, z1
**	umulh	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mulh_u16_m_untied, svuint16_t,
		z0 = svmulh_u16_m (p0, z1, z2),
		z0 = svmulh_m (p0, z1, z2))

/*
** mulh_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	umulh	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mulh_w0_u16_m_tied1, svuint16_t, uint16_t,
		 z0 = svmulh_n_u16_m (p0, z0, x0),
		 z0 = svmulh_m (p0, z0, x0))

/*
** mulh_w0_u16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	umulh	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mulh_w0_u16_m_untied, svuint16_t, uint16_t,
		 z0 = svmulh_n_u16_m (p0, z1, x0),
		 z0 = svmulh_m (p0, z1, x0))

/*
** mulh_11_u16_m_tied1:
**	mov	(z[0-9]+\.h), #11
**	umulh	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mulh_11_u16_m_tied1, svuint16_t,
		z0 = svmulh_n_u16_m (p0, z0, 11),
		z0 = svmulh_m (p0, z0, 11))

/*
** mulh_11_u16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0, z1
**	umulh	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mulh_11_u16_m_untied, svuint16_t,
		z0 = svmulh_n_u16_m (p0, z1, 11),
		z0 = svmulh_m (p0, z1, 11))

/*
** mulh_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	umulh	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mulh_u16_z_tied1, svuint16_t,
		z0 = svmulh_u16_z (p0, z0, z1),
		z0 = svmulh_z (p0, z0, z1))

/*
** mulh_u16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	umulh	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mulh_u16_z_tied2, svuint16_t,
		z0 = svmulh_u16_z (p0, z1, z0),
		z0 = svmulh_z (p0, z1, z0))

/*
** mulh_u16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	umulh	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	umulh	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mulh_u16_z_untied, svuint16_t,
		z0 = svmulh_u16_z (p0, z1, z2),
		z0 = svmulh_z (p0, z1, z2))

/*
** mulh_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	umulh	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mulh_w0_u16_z_tied1, svuint16_t, uint16_t,
		 z0 = svmulh_n_u16_z (p0, z0, x0),
		 z0 = svmulh_z (p0, z0, x0))

/*
** mulh_w0_u16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	umulh	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	umulh	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (mulh_w0_u16_z_untied, svuint16_t, uint16_t,
		 z0 = svmulh_n_u16_z (p0, z1, x0),
		 z0 = svmulh_z (p0, z1, x0))

/*
** mulh_11_u16_z_tied1:
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0\.h, p0/z, z0\.h
**	umulh	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mulh_11_u16_z_tied1, svuint16_t,
		z0 = svmulh_n_u16_z (p0, z0, 11),
		z0 = svmulh_z (p0, z0, 11))

/*
** mulh_11_u16_z_untied:
**	mov	(z[0-9]+\.h), #11
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	umulh	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	umulh	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mulh_11_u16_z_untied, svuint16_t,
		z0 = svmulh_n_u16_z (p0, z1, 11),
		z0 = svmulh_z (p0, z1, 11))

/*
** mulh_u16_x_tied1:
**	umulh	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mulh_u16_x_tied1, svuint16_t,
		z0 = svmulh_u16_x (p0, z0, z1),
		z0 = svmulh_x (p0, z0, z1))

/*
** mulh_u16_x_tied2:
**	umulh	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mulh_u16_x_tied2, svuint16_t,
		z0 = svmulh_u16_x (p0, z1, z0),
		z0 = svmulh_x (p0, z1, z0))

/*
** mulh_u16_x_untied:
** (
**	movprfx	z0, z1
**	umulh	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0, z2
**	umulh	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mulh_u16_x_untied, svuint16_t,
		z0 = svmulh_u16_x (p0, z1, z2),
		z0 = svmulh_x (p0, z1, z2))

/*
** mulh_w0_u16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	umulh	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mulh_w0_u16_x_tied1, svuint16_t, uint16_t,
		 z0 = svmulh_n_u16_x (p0, z0, x0),
		 z0 = svmulh_x (p0, z0, x0))

/*
** mulh_w0_u16_x_untied:
**	mov	z0\.h, w0
**	umulh	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (mulh_w0_u16_x_untied, svuint16_t, uint16_t,
		 z0 = svmulh_n_u16_x (p0, z1, x0),
		 z0 = svmulh_x (p0, z1, x0))

/*
** mulh_11_u16_x_tied1:
**	mov	(z[0-9]+\.h), #11
**	umulh	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mulh_11_u16_x_tied1, svuint16_t,
		z0 = svmulh_n_u16_x (p0, z0, 11),
		z0 = svmulh_x (p0, z0, 11))

/*
** mulh_11_u16_x_untied:
**	mov	z0\.h, #11
**	umulh	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mulh_11_u16_x_untied, svuint16_t,
		z0 = svmulh_n_u16_x (p0, z1, 11),
		z0 = svmulh_x (p0, z1, 11))
