/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshl_u16_m_tied1:
**	urshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (rshl_u16_m_tied1, svuint16_t, svint16_t,
	     z0 = svrshl_u16_m (p0, z0, z4),
	     z0 = svrshl_m (p0, z0, z4))

/*
** rshl_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	urshl	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (rshl_u16_m_tied2, svuint16_t, svint16_t,
		 z0_res = svrshl_u16_m (p0, z4, z0),
		 z0_res = svrshl_m (p0, z4, z0))

/*
** rshl_u16_m_untied:
**	movprfx	z0, z1
**	urshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (rshl_u16_m_untied, svuint16_t, svint16_t,
	     z0 = svrshl_u16_m (p0, z1, z4),
	     z0 = svrshl_m (p0, z1, z4))

/*
** rshl_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	urshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_u16_m_tied1, svuint16_t, int16_t,
		 z0 = svrshl_n_u16_m (p0, z0, x0),
		 z0 = svrshl_m (p0, z0, x0))

/*
** rshl_w0_u16_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	urshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_u16_m_untied, svuint16_t, int16_t,
		 z0 = svrshl_n_u16_m (p0, z1, x0),
		 z0 = svrshl_m (p0, z1, x0))

/*
** rshl_m16_u16_m:
**	urshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (rshl_m16_u16_m, svuint16_t,
		z0 = svrshl_n_u16_m (p0, z0, -16),
		z0 = svrshl_m (p0, z0, -16))

/*
** rshl_m2_u16_m:
**	urshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_u16_m, svuint16_t,
		z0 = svrshl_n_u16_m (p0, z0, -2),
		z0 = svrshl_m (p0, z0, -2))

/*
** rshl_m1_u16_m_tied1:
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_u16_m_tied1, svuint16_t,
		z0 = svrshl_n_u16_m (p0, z0, -1),
		z0 = svrshl_m (p0, z0, -1))

/*
** rshl_m1_u16_m_untied:
**	movprfx	z0, z1
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_u16_m_untied, svuint16_t,
		z0 = svrshl_n_u16_m (p0, z1, -1),
		z0 = svrshl_m (p0, z1, -1))

/*
** rshl_1_u16_m_tied1:
**	lsl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_u16_m_tied1, svuint16_t,
		z0 = svrshl_n_u16_m (p0, z0, 1),
		z0 = svrshl_m (p0, z0, 1))

/*
** rshl_1_u16_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_u16_m_untied, svuint16_t,
		z0 = svrshl_n_u16_m (p0, z1, 1),
		z0 = svrshl_m (p0, z1, 1))

/*
** rshl_2_u16_m:
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_u16_m, svuint16_t,
		z0 = svrshl_n_u16_m (p0, z0, 2),
		z0 = svrshl_m (p0, z0, 2))

/*
** rshl_15_u16_m:
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (rshl_15_u16_m, svuint16_t,
		z0 = svrshl_n_u16_m (p0, z0, 15),
		z0 = svrshl_m (p0, z0, 15))

/*
** rshl_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	urshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (rshl_u16_z_tied1, svuint16_t, svint16_t,
	     z0 = svrshl_u16_z (p0, z0, z4),
	     z0 = svrshl_z (p0, z0, z4))

/*
** rshl_u16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	urshlr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (rshl_u16_z_tied2, svuint16_t, svint16_t,
		 z0_res = svrshl_u16_z (p0, z4, z0),
		 z0_res = svrshl_z (p0, z4, z0))

/*
** rshl_u16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	urshl	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0\.h, p0/z, z4\.h
**	urshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (rshl_u16_z_untied, svuint16_t, svint16_t,
	     z0 = svrshl_u16_z (p0, z1, z4),
	     z0 = svrshl_z (p0, z1, z4))

/*
** rshl_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	urshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_u16_z_tied1, svuint16_t, int16_t,
		 z0 = svrshl_n_u16_z (p0, z0, x0),
		 z0 = svrshl_z (p0, z0, x0))

/*
** rshl_w0_u16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	urshl	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	urshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_u16_z_untied, svuint16_t, int16_t,
		 z0 = svrshl_n_u16_z (p0, z1, x0),
		 z0 = svrshl_z (p0, z1, x0))

/*
** rshl_m16_u16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	urshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (rshl_m16_u16_z, svuint16_t,
		z0 = svrshl_n_u16_z (p0, z0, -16),
		z0 = svrshl_z (p0, z0, -16))

/*
** rshl_m2_u16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	urshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_u16_z, svuint16_t,
		z0 = svrshl_n_u16_z (p0, z0, -2),
		z0 = svrshl_z (p0, z0, -2))

/*
** rshl_m1_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_u16_z_tied1, svuint16_t,
		z0 = svrshl_n_u16_z (p0, z0, -1),
		z0 = svrshl_z (p0, z0, -1))

/*
** rshl_m1_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_u16_z_untied, svuint16_t,
		z0 = svrshl_n_u16_z (p0, z1, -1),
		z0 = svrshl_z (p0, z1, -1))

/*
** rshl_1_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_u16_z_tied1, svuint16_t,
		z0 = svrshl_n_u16_z (p0, z0, 1),
		z0 = svrshl_z (p0, z0, 1))

/*
** rshl_1_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_u16_z_untied, svuint16_t,
		z0 = svrshl_n_u16_z (p0, z1, 1),
		z0 = svrshl_z (p0, z1, 1))

/*
** rshl_2_u16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_u16_z, svuint16_t,
		z0 = svrshl_n_u16_z (p0, z0, 2),
		z0 = svrshl_z (p0, z0, 2))

/*
** rshl_15_u16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (rshl_15_u16_z, svuint16_t,
		z0 = svrshl_n_u16_z (p0, z0, 15),
		z0 = svrshl_z (p0, z0, 15))

/*
** rshl_u16_x_tied1:
**	urshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (rshl_u16_x_tied1, svuint16_t, svint16_t,
	     z0 = svrshl_u16_x (p0, z0, z4),
	     z0 = svrshl_x (p0, z0, z4))

/*
** rshl_u16_x_tied2:
**	urshlr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (rshl_u16_x_tied2, svuint16_t, svint16_t,
		 z0_res = svrshl_u16_x (p0, z4, z0),
		 z0_res = svrshl_x (p0, z4, z0))

/*
** rshl_u16_x_untied:
** (
**	movprfx	z0, z1
**	urshl	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0, z4
**	urshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (rshl_u16_x_untied, svuint16_t, svint16_t,
	     z0 = svrshl_u16_x (p0, z1, z4),
	     z0 = svrshl_x (p0, z1, z4))

/*
** rshl_w0_u16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	urshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_u16_x_tied1, svuint16_t, int16_t,
		 z0 = svrshl_n_u16_x (p0, z0, x0),
		 z0 = svrshl_x (p0, z0, x0))

/*
** rshl_w0_u16_x_untied:
**	mov	z0\.h, w0
**	urshlr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_u16_x_untied, svuint16_t, int16_t,
		 z0 = svrshl_n_u16_x (p0, z1, x0),
		 z0 = svrshl_x (p0, z1, x0))

/*
** rshl_m16_u16_x:
**	urshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (rshl_m16_u16_x, svuint16_t,
		z0 = svrshl_n_u16_x (p0, z0, -16),
		z0 = svrshl_x (p0, z0, -16))

/*
** rshl_m2_u16_x:
**	urshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_u16_x, svuint16_t,
		z0 = svrshl_n_u16_x (p0, z0, -2),
		z0 = svrshl_x (p0, z0, -2))

/*
** rshl_m1_u16_x_tied1:
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_u16_x_tied1, svuint16_t,
		z0 = svrshl_n_u16_x (p0, z0, -1),
		z0 = svrshl_x (p0, z0, -1))

/*
** rshl_m1_u16_x_untied:
**	movprfx	z0, z1
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_u16_x_untied, svuint16_t,
		z0 = svrshl_n_u16_x (p0, z1, -1),
		z0 = svrshl_x (p0, z1, -1))

/*
** rshl_1_u16_x_tied1:
**	add	z0\.h, z0\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rshl_1_u16_x_tied1, svuint16_t,
		z0 = svrshl_n_u16_x (p0, z0, 1),
		z0 = svrshl_x (p0, z0, 1))

/*
** rshl_1_u16_x_untied:
**	add	z0\.h, z1\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rshl_1_u16_x_untied, svuint16_t,
		z0 = svrshl_n_u16_x (p0, z1, 1),
		z0 = svrshl_x (p0, z1, 1))

/*
** rshl_2_u16_x:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_u16_x, svuint16_t,
		z0 = svrshl_n_u16_x (p0, z0, 2),
		z0 = svrshl_x (p0, z0, 2))

/*
** rshl_15_u16_x:
**	lsl	z0\.h, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (rshl_15_u16_x, svuint16_t,
		z0 = svrshl_n_u16_x (p0, z0, 15),
		z0 = svrshl_x (p0, z0, 15))
