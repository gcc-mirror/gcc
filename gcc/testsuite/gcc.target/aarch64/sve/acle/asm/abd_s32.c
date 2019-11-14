/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abd_s32_m_tied1:
**	sabd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (abd_s32_m_tied1, svint32_t,
		z0 = svabd_s32_m (p0, z0, z1),
		z0 = svabd_m (p0, z0, z1))

/*
** abd_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sabd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (abd_s32_m_tied2, svint32_t,
		z0 = svabd_s32_m (p0, z1, z0),
		z0 = svabd_m (p0, z1, z0))

/*
** abd_s32_m_untied:
**	movprfx	z0, z1
**	sabd	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (abd_s32_m_untied, svint32_t,
		z0 = svabd_s32_m (p0, z1, z2),
		z0 = svabd_m (p0, z1, z2))

/*
** abd_w0_s32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	sabd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (abd_w0_s32_m_tied1, svint32_t, int32_t,
		 z0 = svabd_n_s32_m (p0, z0, x0),
		 z0 = svabd_m (p0, z0, x0))

/*
** abd_w0_s32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	sabd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (abd_w0_s32_m_untied, svint32_t, int32_t,
		 z0 = svabd_n_s32_m (p0, z1, x0),
		 z0 = svabd_m (p0, z1, x0))

/*
** abd_1_s32_m_tied1:
**	mov	(z[0-9]+\.s), #1
**	sabd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_s32_m_tied1, svint32_t,
		z0 = svabd_n_s32_m (p0, z0, 1),
		z0 = svabd_m (p0, z0, 1))

/*
** abd_1_s32_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0, z1
**	sabd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_s32_m_untied, svint32_t,
		z0 = svabd_n_s32_m (p0, z1, 1),
		z0 = svabd_m (p0, z1, 1))

/*
** abd_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	sabd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (abd_s32_z_tied1, svint32_t,
		z0 = svabd_s32_z (p0, z0, z1),
		z0 = svabd_z (p0, z0, z1))

/*
** abd_s32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	sabd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (abd_s32_z_tied2, svint32_t,
		z0 = svabd_s32_z (p0, z1, z0),
		z0 = svabd_z (p0, z1, z0))

/*
** abd_s32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	sabd	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	sabd	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (abd_s32_z_untied, svint32_t,
		z0 = svabd_s32_z (p0, z1, z2),
		z0 = svabd_z (p0, z1, z2))

/*
** abd_w0_s32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	sabd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (abd_w0_s32_z_tied1, svint32_t, int32_t,
		 z0 = svabd_n_s32_z (p0, z0, x0),
		 z0 = svabd_z (p0, z0, x0))

/*
** abd_w0_s32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	sabd	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	sabd	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (abd_w0_s32_z_untied, svint32_t, int32_t,
		 z0 = svabd_n_s32_z (p0, z1, x0),
		 z0 = svabd_z (p0, z1, x0))

/*
** abd_1_s32_z_tied1:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z0\.s
**	sabd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_s32_z_tied1, svint32_t,
		z0 = svabd_n_s32_z (p0, z0, 1),
		z0 = svabd_z (p0, z0, 1))

/*
** abd_1_s32_z_untied:
**	mov	(z[0-9]+\.s), #1
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	sabd	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	sabd	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (abd_1_s32_z_untied, svint32_t,
		z0 = svabd_n_s32_z (p0, z1, 1),
		z0 = svabd_z (p0, z1, 1))

/*
** abd_s32_x_tied1:
**	sabd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (abd_s32_x_tied1, svint32_t,
		z0 = svabd_s32_x (p0, z0, z1),
		z0 = svabd_x (p0, z0, z1))

/*
** abd_s32_x_tied2:
**	sabd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (abd_s32_x_tied2, svint32_t,
		z0 = svabd_s32_x (p0, z1, z0),
		z0 = svabd_x (p0, z1, z0))

/*
** abd_s32_x_untied:
** (
**	movprfx	z0, z1
**	sabd	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	sabd	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (abd_s32_x_untied, svint32_t,
		z0 = svabd_s32_x (p0, z1, z2),
		z0 = svabd_x (p0, z1, z2))

/*
** abd_w0_s32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	sabd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (abd_w0_s32_x_tied1, svint32_t, int32_t,
		 z0 = svabd_n_s32_x (p0, z0, x0),
		 z0 = svabd_x (p0, z0, x0))

/*
** abd_w0_s32_x_untied:
**	mov	z0\.s, w0
**	sabd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (abd_w0_s32_x_untied, svint32_t, int32_t,
		 z0 = svabd_n_s32_x (p0, z1, x0),
		 z0 = svabd_x (p0, z1, x0))

/*
** abd_1_s32_x_tied1:
**	mov	(z[0-9]+\.s), #1
**	sabd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_s32_x_tied1, svint32_t,
		z0 = svabd_n_s32_x (p0, z0, 1),
		z0 = svabd_x (p0, z0, 1))

/*
** abd_1_s32_x_untied:
**	mov	z0\.s, #1
**	sabd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (abd_1_s32_x_untied, svint32_t,
		z0 = svabd_n_s32_x (p0, z1, 1),
		z0 = svabd_x (p0, z1, 1))
