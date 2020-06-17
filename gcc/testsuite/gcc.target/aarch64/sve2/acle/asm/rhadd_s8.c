/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rhadd_s8_m_tied1:
**	srhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (rhadd_s8_m_tied1, svint8_t,
		z0 = svrhadd_s8_m (p0, z0, z1),
		z0 = svrhadd_m (p0, z0, z1))

/*
** rhadd_s8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	srhadd	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (rhadd_s8_m_tied2, svint8_t,
		z0 = svrhadd_s8_m (p0, z1, z0),
		z0 = svrhadd_m (p0, z1, z0))

/*
** rhadd_s8_m_untied:
**	movprfx	z0, z1
**	srhadd	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (rhadd_s8_m_untied, svint8_t,
		z0 = svrhadd_s8_m (p0, z1, z2),
		z0 = svrhadd_m (p0, z1, z2))

/*
** rhadd_w0_s8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	srhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (rhadd_w0_s8_m_tied1, svint8_t, int8_t,
		 z0 = svrhadd_n_s8_m (p0, z0, x0),
		 z0 = svrhadd_m (p0, z0, x0))

/*
** rhadd_w0_s8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	srhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (rhadd_w0_s8_m_untied, svint8_t, int8_t,
		 z0 = svrhadd_n_s8_m (p0, z1, x0),
		 z0 = svrhadd_m (p0, z1, x0))

/*
** rhadd_11_s8_m_tied1:
**	mov	(z[0-9]+\.b), #11
**	srhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (rhadd_11_s8_m_tied1, svint8_t,
		z0 = svrhadd_n_s8_m (p0, z0, 11),
		z0 = svrhadd_m (p0, z0, 11))

/*
** rhadd_11_s8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0, z1
**	srhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (rhadd_11_s8_m_untied, svint8_t,
		z0 = svrhadd_n_s8_m (p0, z1, 11),
		z0 = svrhadd_m (p0, z1, 11))

/*
** rhadd_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	srhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (rhadd_s8_z_tied1, svint8_t,
		z0 = svrhadd_s8_z (p0, z0, z1),
		z0 = svrhadd_z (p0, z0, z1))

/*
** rhadd_s8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	srhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (rhadd_s8_z_tied2, svint8_t,
		z0 = svrhadd_s8_z (p0, z1, z0),
		z0 = svrhadd_z (p0, z1, z0))

/*
** rhadd_s8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	srhadd	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	srhadd	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (rhadd_s8_z_untied, svint8_t,
		z0 = svrhadd_s8_z (p0, z1, z2),
		z0 = svrhadd_z (p0, z1, z2))

/*
** rhadd_w0_s8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	srhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (rhadd_w0_s8_z_tied1, svint8_t, int8_t,
		 z0 = svrhadd_n_s8_z (p0, z0, x0),
		 z0 = svrhadd_z (p0, z0, x0))

/*
** rhadd_w0_s8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	srhadd	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	srhadd	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (rhadd_w0_s8_z_untied, svint8_t, int8_t,
		 z0 = svrhadd_n_s8_z (p0, z1, x0),
		 z0 = svrhadd_z (p0, z1, x0))

/*
** rhadd_11_s8_z_tied1:
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0\.b, p0/z, z0\.b
**	srhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (rhadd_11_s8_z_tied1, svint8_t,
		z0 = svrhadd_n_s8_z (p0, z0, 11),
		z0 = svrhadd_z (p0, z0, 11))

/*
** rhadd_11_s8_z_untied:
**	mov	(z[0-9]+\.b), #11
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	srhadd	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	srhadd	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (rhadd_11_s8_z_untied, svint8_t,
		z0 = svrhadd_n_s8_z (p0, z1, 11),
		z0 = svrhadd_z (p0, z1, 11))

/*
** rhadd_s8_x_tied1:
**	srhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (rhadd_s8_x_tied1, svint8_t,
		z0 = svrhadd_s8_x (p0, z0, z1),
		z0 = svrhadd_x (p0, z0, z1))

/*
** rhadd_s8_x_tied2:
**	srhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (rhadd_s8_x_tied2, svint8_t,
		z0 = svrhadd_s8_x (p0, z1, z0),
		z0 = svrhadd_x (p0, z1, z0))

/*
** rhadd_s8_x_untied:
** (
**	movprfx	z0, z1
**	srhadd	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0, z2
**	srhadd	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (rhadd_s8_x_untied, svint8_t,
		z0 = svrhadd_s8_x (p0, z1, z2),
		z0 = svrhadd_x (p0, z1, z2))

/*
** rhadd_w0_s8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	srhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (rhadd_w0_s8_x_tied1, svint8_t, int8_t,
		 z0 = svrhadd_n_s8_x (p0, z0, x0),
		 z0 = svrhadd_x (p0, z0, x0))

/*
** rhadd_w0_s8_x_untied:
**	mov	z0\.b, w0
**	srhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (rhadd_w0_s8_x_untied, svint8_t, int8_t,
		 z0 = svrhadd_n_s8_x (p0, z1, x0),
		 z0 = svrhadd_x (p0, z1, x0))

/*
** rhadd_11_s8_x_tied1:
**	mov	(z[0-9]+\.b), #11
**	srhadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (rhadd_11_s8_x_tied1, svint8_t,
		z0 = svrhadd_n_s8_x (p0, z0, 11),
		z0 = svrhadd_x (p0, z0, 11))

/*
** rhadd_11_s8_x_untied:
**	mov	z0\.b, #11
**	srhadd	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (rhadd_11_s8_x_untied, svint8_t,
		z0 = svrhadd_n_s8_x (p0, z1, 11),
		z0 = svrhadd_x (p0, z1, 11))
