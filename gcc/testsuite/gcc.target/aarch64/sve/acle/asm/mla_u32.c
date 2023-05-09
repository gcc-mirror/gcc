/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mla_u32_m_tied1:
**	mla	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (mla_u32_m_tied1, svuint32_t,
		z0 = svmla_u32_m (p0, z0, z1, z2),
		z0 = svmla_m (p0, z0, z1, z2))

/*
** mla_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	mla	z0\.s, p0/m, \1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (mla_u32_m_tied2, svuint32_t,
		z0 = svmla_u32_m (p0, z1, z0, z2),
		z0 = svmla_m (p0, z1, z0, z2))

/*
** mla_u32_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	mla	z0\.s, p0/m, z2\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (mla_u32_m_tied3, svuint32_t,
		z0 = svmla_u32_m (p0, z1, z2, z0),
		z0 = svmla_m (p0, z1, z2, z0))

/*
** mla_u32_m_untied:
**	movprfx	z0, z1
**	mla	z0\.s, p0/m, z2\.s, z3\.s
**	ret
*/
TEST_UNIFORM_Z (mla_u32_m_untied, svuint32_t,
		z0 = svmla_u32_m (p0, z1, z2, z3),
		z0 = svmla_m (p0, z1, z2, z3))

/*
** mla_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	mla	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mla_w0_u32_m_tied1, svuint32_t, uint32_t,
		 z0 = svmla_n_u32_m (p0, z0, z1, x0),
		 z0 = svmla_m (p0, z0, z1, x0))

/*
** mla_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	mla	z0\.s, p0/m, z2\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mla_w0_u32_m_untied, svuint32_t, uint32_t,
		 z0 = svmla_n_u32_m (p0, z1, z2, x0),
		 z0 = svmla_m (p0, z1, z2, x0))

/*
** mla_11_u32_m_tied1:
**	mov	(z[0-9]+\.s), #11
**	mla	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mla_11_u32_m_tied1, svuint32_t,
		z0 = svmla_n_u32_m (p0, z0, z1, 11),
		z0 = svmla_m (p0, z0, z1, 11))

/*
** mla_11_u32_m_untied:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	mla	z0\.s, p0/m, z2\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mla_11_u32_m_untied, svuint32_t,
		z0 = svmla_n_u32_m (p0, z1, z2, 11),
		z0 = svmla_m (p0, z1, z2, 11))

/*
** mla_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	mla	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (mla_u32_z_tied1, svuint32_t,
		z0 = svmla_u32_z (p0, z0, z1, z2),
		z0 = svmla_z (p0, z0, z1, z2))

/*
** mla_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	mad	z0\.s, p0/m, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mla_u32_z_tied2, svuint32_t,
		z0 = svmla_u32_z (p0, z1, z0, z2),
		z0 = svmla_z (p0, z1, z0, z2))

/*
** mla_u32_z_tied3:
**	movprfx	z0\.s, p0/z, z0\.s
**	mad	z0\.s, p0/m, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mla_u32_z_tied3, svuint32_t,
		z0 = svmla_u32_z (p0, z1, z2, z0),
		z0 = svmla_z (p0, z1, z2, z0))

/*
** mla_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	mla	z0\.s, p0/m, z2\.s, z3\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	mad	z0\.s, p0/m, z3\.s, z1\.s
** |
**	movprfx	z0\.s, p0/z, z3\.s
**	mad	z0\.s, p0/m, z2\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mla_u32_z_untied, svuint32_t,
		z0 = svmla_u32_z (p0, z1, z2, z3),
		z0 = svmla_z (p0, z1, z2, z3))

/*
** mla_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	mla	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mla_w0_u32_z_tied1, svuint32_t, uint32_t,
		 z0 = svmla_n_u32_z (p0, z0, z1, x0),
		 z0 = svmla_z (p0, z0, z1, x0))

/*
** mla_w0_u32_z_tied2:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	mad	z0\.s, p0/m, \1, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (mla_w0_u32_z_tied2, svuint32_t, uint32_t,
		 z0 = svmla_n_u32_z (p0, z1, z0, x0),
		 z0 = svmla_z (p0, z1, z0, x0))

/*
** mla_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	mla	z0\.s, p0/m, z2\.s, \1
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	mad	z0\.s, p0/m, \1, z1\.s
** |
**	movprfx	z0\.s, p0/z, \1
**	mad	z0\.s, p0/m, z2\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (mla_w0_u32_z_untied, svuint32_t, uint32_t,
		 z0 = svmla_n_u32_z (p0, z1, z2, x0),
		 z0 = svmla_z (p0, z1, z2, x0))

/*
** mla_11_u32_z_tied1:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0\.s, p0/z, z0\.s
**	mla	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mla_11_u32_z_tied1, svuint32_t,
		z0 = svmla_n_u32_z (p0, z0, z1, 11),
		z0 = svmla_z (p0, z0, z1, 11))

/*
** mla_11_u32_z_tied2:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0\.s, p0/z, z0\.s
**	mad	z0\.s, p0/m, \1, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mla_11_u32_z_tied2, svuint32_t,
		z0 = svmla_n_u32_z (p0, z1, z0, 11),
		z0 = svmla_z (p0, z1, z0, 11))

/*
** mla_11_u32_z_untied:
**	mov	(z[0-9]+\.s), #11
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	mla	z0\.s, p0/m, z2\.s, \1
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	mad	z0\.s, p0/m, \1, z1\.s
** |
**	movprfx	z0\.s, p0/z, \1
**	mad	z0\.s, p0/m, z2\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mla_11_u32_z_untied, svuint32_t,
		z0 = svmla_n_u32_z (p0, z1, z2, 11),
		z0 = svmla_z (p0, z1, z2, 11))

/*
** mla_u32_x_tied1:
**	mla	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (mla_u32_x_tied1, svuint32_t,
		z0 = svmla_u32_x (p0, z0, z1, z2),
		z0 = svmla_x (p0, z0, z1, z2))

/*
** mla_u32_x_tied2:
**	mad	z0\.s, p0/m, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mla_u32_x_tied2, svuint32_t,
		z0 = svmla_u32_x (p0, z1, z0, z2),
		z0 = svmla_x (p0, z1, z0, z2))

/*
** mla_u32_x_tied3:
**	mad	z0\.s, p0/m, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mla_u32_x_tied3, svuint32_t,
		z0 = svmla_u32_x (p0, z1, z2, z0),
		z0 = svmla_x (p0, z1, z2, z0))

/*
** mla_u32_x_untied:
** (
**	movprfx	z0, z1
**	mla	z0\.s, p0/m, z2\.s, z3\.s
** |
**	movprfx	z0, z2
**	mad	z0\.s, p0/m, z3\.s, z1\.s
** |
**	movprfx	z0, z3
**	mad	z0\.s, p0/m, z2\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mla_u32_x_untied, svuint32_t,
		z0 = svmla_u32_x (p0, z1, z2, z3),
		z0 = svmla_x (p0, z1, z2, z3))

/*
** mla_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	mla	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mla_w0_u32_x_tied1, svuint32_t, uint32_t,
		 z0 = svmla_n_u32_x (p0, z0, z1, x0),
		 z0 = svmla_x (p0, z0, z1, x0))

/*
** mla_w0_u32_x_tied2:
**	mov	(z[0-9]+\.s), w0
**	mad	z0\.s, p0/m, \1, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (mla_w0_u32_x_tied2, svuint32_t, uint32_t,
		 z0 = svmla_n_u32_x (p0, z1, z0, x0),
		 z0 = svmla_x (p0, z1, z0, x0))

/*
** mla_w0_u32_x_untied:
**	mov	z0\.s, w0
**	mad	z0\.s, p0/m, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (mla_w0_u32_x_untied, svuint32_t, uint32_t,
		 z0 = svmla_n_u32_x (p0, z1, z2, x0),
		 z0 = svmla_x (p0, z1, z2, x0))

/*
** mla_11_u32_x_tied1:
**	mov	(z[0-9]+\.s), #11
**	mla	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mla_11_u32_x_tied1, svuint32_t,
		z0 = svmla_n_u32_x (p0, z0, z1, 11),
		z0 = svmla_x (p0, z0, z1, 11))

/*
** mla_11_u32_x_tied2:
**	mov	(z[0-9]+\.s), #11
**	mad	z0\.s, p0/m, \1, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mla_11_u32_x_tied2, svuint32_t,
		z0 = svmla_n_u32_x (p0, z1, z0, 11),
		z0 = svmla_x (p0, z1, z0, 11))

/*
** mla_11_u32_x_untied:
**	mov	z0\.s, #11
**	mad	z0\.s, p0/m, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mla_11_u32_x_untied, svuint32_t,
		z0 = svmla_n_u32_x (p0, z1, z2, 11),
		z0 = svmla_x (p0, z1, z2, 11))
