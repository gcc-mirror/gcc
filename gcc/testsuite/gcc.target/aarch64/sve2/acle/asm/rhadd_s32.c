/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rhadd_s32_m_tied1:
**	srhadd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rhadd_s32_m_tied1, svint32_t,
		z0 = svrhadd_s32_m (p0, z0, z1),
		z0 = svrhadd_m (p0, z0, z1))

/*
** rhadd_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	srhadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (rhadd_s32_m_tied2, svint32_t,
		z0 = svrhadd_s32_m (p0, z1, z0),
		z0 = svrhadd_m (p0, z1, z0))

/*
** rhadd_s32_m_untied:
**	movprfx	z0, z1
**	srhadd	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (rhadd_s32_m_untied, svint32_t,
		z0 = svrhadd_s32_m (p0, z1, z2),
		z0 = svrhadd_m (p0, z1, z2))

/*
** rhadd_w0_s32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	srhadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (rhadd_w0_s32_m_tied1, svint32_t, int32_t,
		 z0 = svrhadd_n_s32_m (p0, z0, x0),
		 z0 = svrhadd_m (p0, z0, x0))

/*
** rhadd_w0_s32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	srhadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (rhadd_w0_s32_m_untied, svint32_t, int32_t,
		 z0 = svrhadd_n_s32_m (p0, z1, x0),
		 z0 = svrhadd_m (p0, z1, x0))

/*
** rhadd_11_s32_m_tied1:
**	mov	(z[0-9]+\.s), #11
**	srhadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (rhadd_11_s32_m_tied1, svint32_t,
		z0 = svrhadd_n_s32_m (p0, z0, 11),
		z0 = svrhadd_m (p0, z0, 11))

/*
** rhadd_11_s32_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	srhadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (rhadd_11_s32_m_untied, svint32_t,
		z0 = svrhadd_n_s32_m (p0, z1, 11),
		z0 = svrhadd_m (p0, z1, 11))

/*
** rhadd_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	srhadd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rhadd_s32_z_tied1, svint32_t,
		z0 = svrhadd_s32_z (p0, z0, z1),
		z0 = svrhadd_z (p0, z0, z1))

/*
** rhadd_s32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	srhadd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rhadd_s32_z_tied2, svint32_t,
		z0 = svrhadd_s32_z (p0, z1, z0),
		z0 = svrhadd_z (p0, z1, z0))

/*
** rhadd_s32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	srhadd	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	srhadd	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (rhadd_s32_z_untied, svint32_t,
		z0 = svrhadd_s32_z (p0, z1, z2),
		z0 = svrhadd_z (p0, z1, z2))

/*
** rhadd_w0_s32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	srhadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (rhadd_w0_s32_z_tied1, svint32_t, int32_t,
		 z0 = svrhadd_n_s32_z (p0, z0, x0),
		 z0 = svrhadd_z (p0, z0, x0))

/*
** rhadd_w0_s32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	srhadd	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	srhadd	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (rhadd_w0_s32_z_untied, svint32_t, int32_t,
		 z0 = svrhadd_n_s32_z (p0, z1, x0),
		 z0 = svrhadd_z (p0, z1, x0))

/*
** rhadd_11_s32_z_tied1:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0\.s, p0/z, z0\.s
**	srhadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (rhadd_11_s32_z_tied1, svint32_t,
		z0 = svrhadd_n_s32_z (p0, z0, 11),
		z0 = svrhadd_z (p0, z0, 11))

/*
** rhadd_11_s32_z_untied:
**	mov	(z[0-9]+\.s), #11
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	srhadd	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	srhadd	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (rhadd_11_s32_z_untied, svint32_t,
		z0 = svrhadd_n_s32_z (p0, z1, 11),
		z0 = svrhadd_z (p0, z1, 11))

/*
** rhadd_s32_x_tied1:
**	srhadd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rhadd_s32_x_tied1, svint32_t,
		z0 = svrhadd_s32_x (p0, z0, z1),
		z0 = svrhadd_x (p0, z0, z1))

/*
** rhadd_s32_x_tied2:
**	srhadd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rhadd_s32_x_tied2, svint32_t,
		z0 = svrhadd_s32_x (p0, z1, z0),
		z0 = svrhadd_x (p0, z1, z0))

/*
** rhadd_s32_x_untied:
** (
**	movprfx	z0, z1
**	srhadd	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	srhadd	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (rhadd_s32_x_untied, svint32_t,
		z0 = svrhadd_s32_x (p0, z1, z2),
		z0 = svrhadd_x (p0, z1, z2))

/*
** rhadd_w0_s32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	srhadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (rhadd_w0_s32_x_tied1, svint32_t, int32_t,
		 z0 = svrhadd_n_s32_x (p0, z0, x0),
		 z0 = svrhadd_x (p0, z0, x0))

/*
** rhadd_w0_s32_x_untied:
**	mov	z0\.s, w0
**	srhadd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (rhadd_w0_s32_x_untied, svint32_t, int32_t,
		 z0 = svrhadd_n_s32_x (p0, z1, x0),
		 z0 = svrhadd_x (p0, z1, x0))

/*
** rhadd_11_s32_x_tied1:
**	mov	(z[0-9]+\.s), #11
**	srhadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (rhadd_11_s32_x_tied1, svint32_t,
		z0 = svrhadd_n_s32_x (p0, z0, 11),
		z0 = svrhadd_x (p0, z0, 11))

/*
** rhadd_11_s32_x_untied:
**	mov	z0\.s, #11
**	srhadd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rhadd_11_s32_x_untied, svint32_t,
		z0 = svrhadd_n_s32_x (p0, z1, 11),
		z0 = svrhadd_x (p0, z1, 11))
