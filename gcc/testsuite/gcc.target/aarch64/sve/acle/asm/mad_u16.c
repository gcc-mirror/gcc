/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mad_u16_m_tied1:
**	mad	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mad_u16_m_tied1, svuint16_t,
		z0 = svmad_u16_m (p0, z0, z1, z2),
		z0 = svmad_m (p0, z0, z1, z2))

/*
** mad_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	mad	z0\.h, p0/m, \1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mad_u16_m_tied2, svuint16_t,
		z0 = svmad_u16_m (p0, z1, z0, z2),
		z0 = svmad_m (p0, z1, z0, z2))

/*
** mad_u16_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	mad	z0\.h, p0/m, z2\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (mad_u16_m_tied3, svuint16_t,
		z0 = svmad_u16_m (p0, z1, z2, z0),
		z0 = svmad_m (p0, z1, z2, z0))

/*
** mad_u16_m_untied:
**	movprfx	z0, z1
**	mad	z0\.h, p0/m, z2\.h, z3\.h
**	ret
*/
TEST_UNIFORM_Z (mad_u16_m_untied, svuint16_t,
		z0 = svmad_u16_m (p0, z1, z2, z3),
		z0 = svmad_m (p0, z1, z2, z3))

/*
** mad_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	mad	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mad_w0_u16_m_tied1, svuint16_t, uint16_t,
		 z0 = svmad_n_u16_m (p0, z0, z1, x0),
		 z0 = svmad_m (p0, z0, z1, x0))

/*
** mad_w0_u16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	mad	z0\.h, p0/m, z2\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mad_w0_u16_m_untied, svuint16_t, uint16_t,
		 z0 = svmad_n_u16_m (p0, z1, z2, x0),
		 z0 = svmad_m (p0, z1, z2, x0))

/*
** mad_11_u16_m_tied1:
**	mov	(z[0-9]+\.h), #11
**	mad	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mad_11_u16_m_tied1, svuint16_t,
		z0 = svmad_n_u16_m (p0, z0, z1, 11),
		z0 = svmad_m (p0, z0, z1, 11))

/*
** mad_11_u16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0, z1
**	mad	z0\.h, p0/m, z2\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mad_11_u16_m_untied, svuint16_t,
		z0 = svmad_n_u16_m (p0, z1, z2, 11),
		z0 = svmad_m (p0, z1, z2, 11))

/*
** mad_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	mad	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mad_u16_z_tied1, svuint16_t,
		z0 = svmad_u16_z (p0, z0, z1, z2),
		z0 = svmad_z (p0, z0, z1, z2))

/*
** mad_u16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	mad	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mad_u16_z_tied2, svuint16_t,
		z0 = svmad_u16_z (p0, z1, z0, z2),
		z0 = svmad_z (p0, z1, z0, z2))

/*
** mad_u16_z_tied3:
**	movprfx	z0\.h, p0/z, z0\.h
**	mla	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mad_u16_z_tied3, svuint16_t,
		z0 = svmad_u16_z (p0, z1, z2, z0),
		z0 = svmad_z (p0, z1, z2, z0))

/*
** mad_u16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	mad	z0\.h, p0/m, z2\.h, z3\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	mad	z0\.h, p0/m, z1\.h, z3\.h
** |
**	movprfx	z0\.h, p0/z, z3\.h
**	mla	z0\.h, p0/m, z1\.h, z2\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mad_u16_z_untied, svuint16_t,
		z0 = svmad_u16_z (p0, z1, z2, z3),
		z0 = svmad_z (p0, z1, z2, z3))

/*
** mad_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	mad	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mad_w0_u16_z_tied1, svuint16_t, uint16_t,
		 z0 = svmad_n_u16_z (p0, z0, z1, x0),
		 z0 = svmad_z (p0, z0, z1, x0))

/*
** mad_w0_u16_z_tied2:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	mad	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mad_w0_u16_z_tied2, svuint16_t, uint16_t,
		 z0 = svmad_n_u16_z (p0, z1, z0, x0),
		 z0 = svmad_z (p0, z1, z0, x0))

/*
** mad_w0_u16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	mad	z0\.h, p0/m, z2\.h, \1
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	mad	z0\.h, p0/m, z1\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	mla	z0\.h, p0/m, z1\.h, z2\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (mad_w0_u16_z_untied, svuint16_t, uint16_t,
		 z0 = svmad_n_u16_z (p0, z1, z2, x0),
		 z0 = svmad_z (p0, z1, z2, x0))

/*
** mad_11_u16_z_tied1:
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0\.h, p0/z, z0\.h
**	mad	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mad_11_u16_z_tied1, svuint16_t,
		z0 = svmad_n_u16_z (p0, z0, z1, 11),
		z0 = svmad_z (p0, z0, z1, 11))

/*
** mad_11_u16_z_tied2:
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0\.h, p0/z, z0\.h
**	mad	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mad_11_u16_z_tied2, svuint16_t,
		z0 = svmad_n_u16_z (p0, z1, z0, 11),
		z0 = svmad_z (p0, z1, z0, 11))

/*
** mad_11_u16_z_untied:
**	mov	(z[0-9]+\.h), #11
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	mad	z0\.h, p0/m, z2\.h, \1
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	mad	z0\.h, p0/m, z1\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	mla	z0\.h, p0/m, z1\.h, z2\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mad_11_u16_z_untied, svuint16_t,
		z0 = svmad_n_u16_z (p0, z1, z2, 11),
		z0 = svmad_z (p0, z1, z2, 11))

/*
** mad_u16_x_tied1:
**	mad	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mad_u16_x_tied1, svuint16_t,
		z0 = svmad_u16_x (p0, z0, z1, z2),
		z0 = svmad_x (p0, z0, z1, z2))

/*
** mad_u16_x_tied2:
**	mad	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mad_u16_x_tied2, svuint16_t,
		z0 = svmad_u16_x (p0, z1, z0, z2),
		z0 = svmad_x (p0, z1, z0, z2))

/*
** mad_u16_x_tied3:
**	mla	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mad_u16_x_tied3, svuint16_t,
		z0 = svmad_u16_x (p0, z1, z2, z0),
		z0 = svmad_x (p0, z1, z2, z0))

/*
** mad_u16_x_untied:
** (
**	movprfx	z0, z1
**	mad	z0\.h, p0/m, z2\.h, z3\.h
** |
**	movprfx	z0, z2
**	mad	z0\.h, p0/m, z1\.h, z3\.h
** |
**	movprfx	z0, z3
**	mla	z0\.h, p0/m, z1\.h, z2\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mad_u16_x_untied, svuint16_t,
		z0 = svmad_u16_x (p0, z1, z2, z3),
		z0 = svmad_x (p0, z1, z2, z3))

/*
** mad_w0_u16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	mad	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mad_w0_u16_x_tied1, svuint16_t, uint16_t,
		 z0 = svmad_n_u16_x (p0, z0, z1, x0),
		 z0 = svmad_x (p0, z0, z1, x0))

/*
** mad_w0_u16_x_tied2:
**	mov	(z[0-9]+\.h), w0
**	mad	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mad_w0_u16_x_tied2, svuint16_t, uint16_t,
		 z0 = svmad_n_u16_x (p0, z1, z0, x0),
		 z0 = svmad_x (p0, z1, z0, x0))

/*
** mad_w0_u16_x_untied:
**	mov	z0\.h, w0
**	mla	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_ZX (mad_w0_u16_x_untied, svuint16_t, uint16_t,
		 z0 = svmad_n_u16_x (p0, z1, z2, x0),
		 z0 = svmad_x (p0, z1, z2, x0))

/*
** mad_11_u16_x_tied1:
**	mov	(z[0-9]+\.h), #11
**	mad	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mad_11_u16_x_tied1, svuint16_t,
		z0 = svmad_n_u16_x (p0, z0, z1, 11),
		z0 = svmad_x (p0, z0, z1, 11))

/*
** mad_11_u16_x_tied2:
**	mov	(z[0-9]+\.h), #11
**	mad	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mad_11_u16_x_tied2, svuint16_t,
		z0 = svmad_n_u16_x (p0, z1, z0, 11),
		z0 = svmad_x (p0, z1, z0, 11))

/*
** mad_11_u16_x_untied:
**	mov	z0\.h, #11
**	mla	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mad_11_u16_x_untied, svuint16_t,
		z0 = svmad_n_u16_x (p0, z1, z2, 11),
		z0 = svmad_x (p0, z1, z2, 11))
