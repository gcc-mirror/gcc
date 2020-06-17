/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshl_u16_m_tied1:
**	uqrshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qrshl_u16_m_tied1, svuint16_t, svint16_t,
	     z0 = svqrshl_u16_m (p0, z0, z4),
	     z0 = svqrshl_m (p0, z0, z4))

/*
** qrshl_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uqrshl	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (qrshl_u16_m_tied2, svuint16_t, svint16_t,
		 z0_res = svqrshl_u16_m (p0, z4, z0),
		 z0_res = svqrshl_m (p0, z4, z0))

/*
** qrshl_u16_m_untied:
**	movprfx	z0, z1
**	uqrshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qrshl_u16_m_untied, svuint16_t, svint16_t,
	     z0 = svqrshl_u16_m (p0, z1, z4),
	     z0 = svqrshl_m (p0, z1, z4))

/*
** qrshl_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	uqrshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u16_m_tied1, svuint16_t, int16_t,
		 z0 = svqrshl_n_u16_m (p0, z0, x0),
		 z0 = svqrshl_m (p0, z0, x0))

/*
** qrshl_w0_u16_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	uqrshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u16_m_untied, svuint16_t, int16_t,
		 z0 = svqrshl_n_u16_m (p0, z1, x0),
		 z0 = svqrshl_m (p0, z1, x0))

/*
** qrshl_m16_u16_m:
**	urshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (qrshl_m16_u16_m, svuint16_t,
		z0 = svqrshl_n_u16_m (p0, z0, -16),
		z0 = svqrshl_m (p0, z0, -16))

/*
** qrshl_m2_u16_m:
**	urshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_u16_m, svuint16_t,
		z0 = svqrshl_n_u16_m (p0, z0, -2),
		z0 = svqrshl_m (p0, z0, -2))

/*
** qrshl_m1_u16_m_tied1:
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u16_m_tied1, svuint16_t,
		z0 = svqrshl_n_u16_m (p0, z0, -1),
		z0 = svqrshl_m (p0, z0, -1))

/*
** qrshl_m1_u16_m_untied:
**	movprfx	z0, z1
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u16_m_untied, svuint16_t,
		z0 = svqrshl_n_u16_m (p0, z1, -1),
		z0 = svqrshl_m (p0, z1, -1))

/*
** qrshl_1_u16_m_tied1:
**	uqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u16_m_tied1, svuint16_t,
		z0 = svqrshl_n_u16_m (p0, z0, 1),
		z0 = svqrshl_m (p0, z0, 1))

/*
** qrshl_1_u16_m_untied:
**	movprfx	z0, z1
**	uqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u16_m_untied, svuint16_t,
		z0 = svqrshl_n_u16_m (p0, z1, 1),
		z0 = svqrshl_m (p0, z1, 1))

/*
** qrshl_2_u16_m:
**	uqshl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_u16_m, svuint16_t,
		z0 = svqrshl_n_u16_m (p0, z0, 2),
		z0 = svqrshl_m (p0, z0, 2))

/*
** qrshl_15_u16_m:
**	uqshl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (qrshl_15_u16_m, svuint16_t,
		z0 = svqrshl_n_u16_m (p0, z0, 15),
		z0 = svqrshl_m (p0, z0, 15))

/*
** qrshl_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	uqrshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qrshl_u16_z_tied1, svuint16_t, svint16_t,
	     z0 = svqrshl_u16_z (p0, z0, z4),
	     z0 = svqrshl_z (p0, z0, z4))

/*
** qrshl_u16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	uqrshlr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (qrshl_u16_z_tied2, svuint16_t, svint16_t,
		 z0_res = svqrshl_u16_z (p0, z4, z0),
		 z0_res = svqrshl_z (p0, z4, z0))

/*
** qrshl_u16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	uqrshl	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0\.h, p0/z, z4\.h
**	uqrshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (qrshl_u16_z_untied, svuint16_t, svint16_t,
	     z0 = svqrshl_u16_z (p0, z1, z4),
	     z0 = svqrshl_z (p0, z1, z4))

/*
** qrshl_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	uqrshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u16_z_tied1, svuint16_t, int16_t,
		 z0 = svqrshl_n_u16_z (p0, z0, x0),
		 z0 = svqrshl_z (p0, z0, x0))

/*
** qrshl_w0_u16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	uqrshl	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	uqrshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u16_z_untied, svuint16_t, int16_t,
		 z0 = svqrshl_n_u16_z (p0, z1, x0),
		 z0 = svqrshl_z (p0, z1, x0))

/*
** qrshl_m16_u16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	urshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (qrshl_m16_u16_z, svuint16_t,
		z0 = svqrshl_n_u16_z (p0, z0, -16),
		z0 = svqrshl_z (p0, z0, -16))

/*
** qrshl_m2_u16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	urshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_u16_z, svuint16_t,
		z0 = svqrshl_n_u16_z (p0, z0, -2),
		z0 = svqrshl_z (p0, z0, -2))

/*
** qrshl_m1_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u16_z_tied1, svuint16_t,
		z0 = svqrshl_n_u16_z (p0, z0, -1),
		z0 = svqrshl_z (p0, z0, -1))

/*
** qrshl_m1_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u16_z_untied, svuint16_t,
		z0 = svqrshl_n_u16_z (p0, z1, -1),
		z0 = svqrshl_z (p0, z1, -1))

/*
** qrshl_1_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	uqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u16_z_tied1, svuint16_t,
		z0 = svqrshl_n_u16_z (p0, z0, 1),
		z0 = svqrshl_z (p0, z0, 1))

/*
** qrshl_1_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	uqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u16_z_untied, svuint16_t,
		z0 = svqrshl_n_u16_z (p0, z1, 1),
		z0 = svqrshl_z (p0, z1, 1))

/*
** qrshl_2_u16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	uqshl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_u16_z, svuint16_t,
		z0 = svqrshl_n_u16_z (p0, z0, 2),
		z0 = svqrshl_z (p0, z0, 2))

/*
** qrshl_15_u16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	uqshl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (qrshl_15_u16_z, svuint16_t,
		z0 = svqrshl_n_u16_z (p0, z0, 15),
		z0 = svqrshl_z (p0, z0, 15))

/*
** qrshl_u16_x_tied1:
**	uqrshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qrshl_u16_x_tied1, svuint16_t, svint16_t,
	     z0 = svqrshl_u16_x (p0, z0, z4),
	     z0 = svqrshl_x (p0, z0, z4))

/*
** qrshl_u16_x_tied2:
**	uqrshlr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (qrshl_u16_x_tied2, svuint16_t, svint16_t,
		 z0_res = svqrshl_u16_x (p0, z4, z0),
		 z0_res = svqrshl_x (p0, z4, z0))

/*
** qrshl_u16_x_untied:
** (
**	movprfx	z0, z1
**	uqrshl	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0, z4
**	uqrshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (qrshl_u16_x_untied, svuint16_t, svint16_t,
	     z0 = svqrshl_u16_x (p0, z1, z4),
	     z0 = svqrshl_x (p0, z1, z4))

/*
** qrshl_w0_u16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	uqrshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u16_x_tied1, svuint16_t, int16_t,
		 z0 = svqrshl_n_u16_x (p0, z0, x0),
		 z0 = svqrshl_x (p0, z0, x0))

/*
** qrshl_w0_u16_x_untied:
**	mov	z0\.h, w0
**	uqrshlr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u16_x_untied, svuint16_t, int16_t,
		 z0 = svqrshl_n_u16_x (p0, z1, x0),
		 z0 = svqrshl_x (p0, z1, x0))

/*
** qrshl_m16_u16_x:
**	urshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (qrshl_m16_u16_x, svuint16_t,
		z0 = svqrshl_n_u16_x (p0, z0, -16),
		z0 = svqrshl_x (p0, z0, -16))

/*
** qrshl_m2_u16_x:
**	urshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_u16_x, svuint16_t,
		z0 = svqrshl_n_u16_x (p0, z0, -2),
		z0 = svqrshl_x (p0, z0, -2))

/*
** qrshl_m1_u16_x_tied1:
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u16_x_tied1, svuint16_t,
		z0 = svqrshl_n_u16_x (p0, z0, -1),
		z0 = svqrshl_x (p0, z0, -1))

/*
** qrshl_m1_u16_x_untied:
**	movprfx	z0, z1
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u16_x_untied, svuint16_t,
		z0 = svqrshl_n_u16_x (p0, z1, -1),
		z0 = svqrshl_x (p0, z1, -1))

/*
** qrshl_1_u16_x_tied1:
**	uqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u16_x_tied1, svuint16_t,
		z0 = svqrshl_n_u16_x (p0, z0, 1),
		z0 = svqrshl_x (p0, z0, 1))

/*
** qrshl_1_u16_x_untied:
**	movprfx	z0, z1
**	uqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u16_x_untied, svuint16_t,
		z0 = svqrshl_n_u16_x (p0, z1, 1),
		z0 = svqrshl_x (p0, z1, 1))

/*
** qrshl_2_u16_x:
**	uqshl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_u16_x, svuint16_t,
		z0 = svqrshl_n_u16_x (p0, z0, 2),
		z0 = svqrshl_x (p0, z0, 2))

/*
** qrshl_15_u16_x:
**	uqshl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (qrshl_15_u16_x, svuint16_t,
		z0 = svqrshl_n_u16_x (p0, z0, 15),
		z0 = svqrshl_x (p0, z0, 15))
