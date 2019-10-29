/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mulh_u32_m_tied1:
**	umulh	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mulh_u32_m_tied1, svuint32_t,
		z0 = svmulh_u32_m (p0, z0, z1),
		z0 = svmulh_m (p0, z0, z1))

/*
** mulh_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	umulh	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (mulh_u32_m_tied2, svuint32_t,
		z0 = svmulh_u32_m (p0, z1, z0),
		z0 = svmulh_m (p0, z1, z0))

/*
** mulh_u32_m_untied:
**	movprfx	z0, z1
**	umulh	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (mulh_u32_m_untied, svuint32_t,
		z0 = svmulh_u32_m (p0, z1, z2),
		z0 = svmulh_m (p0, z1, z2))

/*
** mulh_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	umulh	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mulh_w0_u32_m_tied1, svuint32_t, uint32_t,
		 z0 = svmulh_n_u32_m (p0, z0, x0),
		 z0 = svmulh_m (p0, z0, x0))

/*
** mulh_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	umulh	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mulh_w0_u32_m_untied, svuint32_t, uint32_t,
		 z0 = svmulh_n_u32_m (p0, z1, x0),
		 z0 = svmulh_m (p0, z1, x0))

/*
** mulh_11_u32_m_tied1:
**	mov	(z[0-9]+\.s), #11
**	umulh	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mulh_11_u32_m_tied1, svuint32_t,
		z0 = svmulh_n_u32_m (p0, z0, 11),
		z0 = svmulh_m (p0, z0, 11))

/*
** mulh_11_u32_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	umulh	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mulh_11_u32_m_untied, svuint32_t,
		z0 = svmulh_n_u32_m (p0, z1, 11),
		z0 = svmulh_m (p0, z1, 11))

/*
** mulh_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	umulh	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mulh_u32_z_tied1, svuint32_t,
		z0 = svmulh_u32_z (p0, z0, z1),
		z0 = svmulh_z (p0, z0, z1))

/*
** mulh_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	umulh	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mulh_u32_z_tied2, svuint32_t,
		z0 = svmulh_u32_z (p0, z1, z0),
		z0 = svmulh_z (p0, z1, z0))

/*
** mulh_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	umulh	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	umulh	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mulh_u32_z_untied, svuint32_t,
		z0 = svmulh_u32_z (p0, z1, z2),
		z0 = svmulh_z (p0, z1, z2))

/*
** mulh_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	umulh	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mulh_w0_u32_z_tied1, svuint32_t, uint32_t,
		 z0 = svmulh_n_u32_z (p0, z0, x0),
		 z0 = svmulh_z (p0, z0, x0))

/*
** mulh_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	umulh	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	umulh	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (mulh_w0_u32_z_untied, svuint32_t, uint32_t,
		 z0 = svmulh_n_u32_z (p0, z1, x0),
		 z0 = svmulh_z (p0, z1, x0))

/*
** mulh_11_u32_z_tied1:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0\.s, p0/z, z0\.s
**	umulh	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mulh_11_u32_z_tied1, svuint32_t,
		z0 = svmulh_n_u32_z (p0, z0, 11),
		z0 = svmulh_z (p0, z0, 11))

/*
** mulh_11_u32_z_untied:
**	mov	(z[0-9]+\.s), #11
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	umulh	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	umulh	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mulh_11_u32_z_untied, svuint32_t,
		z0 = svmulh_n_u32_z (p0, z1, 11),
		z0 = svmulh_z (p0, z1, 11))

/*
** mulh_u32_x_tied1:
**	umulh	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mulh_u32_x_tied1, svuint32_t,
		z0 = svmulh_u32_x (p0, z0, z1),
		z0 = svmulh_x (p0, z0, z1))

/*
** mulh_u32_x_tied2:
**	umulh	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mulh_u32_x_tied2, svuint32_t,
		z0 = svmulh_u32_x (p0, z1, z0),
		z0 = svmulh_x (p0, z1, z0))

/*
** mulh_u32_x_untied:
** (
**	movprfx	z0, z1
**	umulh	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	umulh	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mulh_u32_x_untied, svuint32_t,
		z0 = svmulh_u32_x (p0, z1, z2),
		z0 = svmulh_x (p0, z1, z2))

/*
** mulh_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	umulh	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mulh_w0_u32_x_tied1, svuint32_t, uint32_t,
		 z0 = svmulh_n_u32_x (p0, z0, x0),
		 z0 = svmulh_x (p0, z0, x0))

/*
** mulh_w0_u32_x_untied:
**	mov	z0\.s, w0
**	umulh	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (mulh_w0_u32_x_untied, svuint32_t, uint32_t,
		 z0 = svmulh_n_u32_x (p0, z1, x0),
		 z0 = svmulh_x (p0, z1, x0))

/*
** mulh_11_u32_x_tied1:
**	mov	(z[0-9]+\.s), #11
**	umulh	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mulh_11_u32_x_tied1, svuint32_t,
		z0 = svmulh_n_u32_x (p0, z0, 11),
		z0 = svmulh_x (p0, z0, 11))

/*
** mulh_11_u32_x_untied:
**	mov	z0\.s, #11
**	umulh	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mulh_11_u32_x_untied, svuint32_t,
		z0 = svmulh_n_u32_x (p0, z1, 11),
		z0 = svmulh_x (p0, z1, 11))
