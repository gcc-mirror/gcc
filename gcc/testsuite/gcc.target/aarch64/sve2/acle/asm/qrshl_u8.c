/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshl_u8_m_tied1:
**	uqrshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (qrshl_u8_m_tied1, svuint8_t, svint8_t,
	     z0 = svqrshl_u8_m (p0, z0, z4),
	     z0 = svqrshl_m (p0, z0, z4))

/*
** qrshl_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uqrshl	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (qrshl_u8_m_tied2, svuint8_t, svint8_t,
		 z0_res = svqrshl_u8_m (p0, z4, z0),
		 z0_res = svqrshl_m (p0, z4, z0))

/*
** qrshl_u8_m_untied:
**	movprfx	z0, z1
**	uqrshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (qrshl_u8_m_untied, svuint8_t, svint8_t,
	     z0 = svqrshl_u8_m (p0, z1, z4),
	     z0 = svqrshl_m (p0, z1, z4))

/*
** qrshl_w0_u8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	uqrshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u8_m_tied1, svuint8_t, int8_t,
		 z0 = svqrshl_n_u8_m (p0, z0, x0),
		 z0 = svqrshl_m (p0, z0, x0))

/*
** qrshl_w0_u8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	uqrshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u8_m_untied, svuint8_t, int8_t,
		 z0 = svqrshl_n_u8_m (p0, z1, x0),
		 z0 = svqrshl_m (p0, z1, x0))

/*
** qrshl_m8_u8_m:
**	urshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (qrshl_m8_u8_m, svuint8_t,
		z0 = svqrshl_n_u8_m (p0, z0, -8),
		z0 = svqrshl_m (p0, z0, -8))

/*
** qrshl_m2_u8_m:
**	urshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_u8_m, svuint8_t,
		z0 = svqrshl_n_u8_m (p0, z0, -2),
		z0 = svqrshl_m (p0, z0, -2))

/*
** qrshl_m1_u8_m_tied1:
**	urshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u8_m_tied1, svuint8_t,
		z0 = svqrshl_n_u8_m (p0, z0, -1),
		z0 = svqrshl_m (p0, z0, -1))

/*
** qrshl_m1_u8_m_untied:
**	movprfx	z0, z1
**	urshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u8_m_untied, svuint8_t,
		z0 = svqrshl_n_u8_m (p0, z1, -1),
		z0 = svqrshl_m (p0, z1, -1))

/*
** qrshl_1_u8_m_tied1:
**	uqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u8_m_tied1, svuint8_t,
		z0 = svqrshl_n_u8_m (p0, z0, 1),
		z0 = svqrshl_m (p0, z0, 1))

/*
** qrshl_1_u8_m_untied:
**	movprfx	z0, z1
**	uqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u8_m_untied, svuint8_t,
		z0 = svqrshl_n_u8_m (p0, z1, 1),
		z0 = svqrshl_m (p0, z1, 1))

/*
** qrshl_2_u8_m:
**	uqshl	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_u8_m, svuint8_t,
		z0 = svqrshl_n_u8_m (p0, z0, 2),
		z0 = svqrshl_m (p0, z0, 2))

/*
** qrshl_7_u8_m:
**	uqshl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (qrshl_7_u8_m, svuint8_t,
		z0 = svqrshl_n_u8_m (p0, z0, 7),
		z0 = svqrshl_m (p0, z0, 7))

/*
** qrshl_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	uqrshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (qrshl_u8_z_tied1, svuint8_t, svint8_t,
	     z0 = svqrshl_u8_z (p0, z0, z4),
	     z0 = svqrshl_z (p0, z0, z4))

/*
** qrshl_u8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	uqrshlr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z_REV (qrshl_u8_z_tied2, svuint8_t, svint8_t,
		 z0_res = svqrshl_u8_z (p0, z4, z0),
		 z0_res = svqrshl_z (p0, z4, z0))

/*
** qrshl_u8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uqrshl	z0\.b, p0/m, z0\.b, z4\.b
** |
**	movprfx	z0\.b, p0/z, z4\.b
**	uqrshlr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_DUAL_Z (qrshl_u8_z_untied, svuint8_t, svint8_t,
	     z0 = svqrshl_u8_z (p0, z1, z4),
	     z0 = svqrshl_z (p0, z1, z4))

/*
** qrshl_w0_u8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	uqrshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u8_z_tied1, svuint8_t, int8_t,
		 z0 = svqrshl_n_u8_z (p0, z0, x0),
		 z0 = svqrshl_z (p0, z0, x0))

/*
** qrshl_w0_u8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uqrshl	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	uqrshlr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u8_z_untied, svuint8_t, int8_t,
		 z0 = svqrshl_n_u8_z (p0, z1, x0),
		 z0 = svqrshl_z (p0, z1, x0))

/*
** qrshl_m8_u8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	urshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (qrshl_m8_u8_z, svuint8_t,
		z0 = svqrshl_n_u8_z (p0, z0, -8),
		z0 = svqrshl_z (p0, z0, -8))

/*
** qrshl_m2_u8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	urshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_u8_z, svuint8_t,
		z0 = svqrshl_n_u8_z (p0, z0, -2),
		z0 = svqrshl_z (p0, z0, -2))

/*
** qrshl_m1_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	urshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u8_z_tied1, svuint8_t,
		z0 = svqrshl_n_u8_z (p0, z0, -1),
		z0 = svqrshl_z (p0, z0, -1))

/*
** qrshl_m1_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	urshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u8_z_untied, svuint8_t,
		z0 = svqrshl_n_u8_z (p0, z1, -1),
		z0 = svqrshl_z (p0, z1, -1))

/*
** qrshl_1_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	uqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u8_z_tied1, svuint8_t,
		z0 = svqrshl_n_u8_z (p0, z0, 1),
		z0 = svqrshl_z (p0, z0, 1))

/*
** qrshl_1_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	uqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u8_z_untied, svuint8_t,
		z0 = svqrshl_n_u8_z (p0, z1, 1),
		z0 = svqrshl_z (p0, z1, 1))

/*
** qrshl_2_u8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	uqshl	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_u8_z, svuint8_t,
		z0 = svqrshl_n_u8_z (p0, z0, 2),
		z0 = svqrshl_z (p0, z0, 2))

/*
** qrshl_7_u8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	uqshl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (qrshl_7_u8_z, svuint8_t,
		z0 = svqrshl_n_u8_z (p0, z0, 7),
		z0 = svqrshl_z (p0, z0, 7))

/*
** qrshl_u8_x_tied1:
**	uqrshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (qrshl_u8_x_tied1, svuint8_t, svint8_t,
	     z0 = svqrshl_u8_x (p0, z0, z4),
	     z0 = svqrshl_x (p0, z0, z4))

/*
** qrshl_u8_x_tied2:
**	uqrshlr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z_REV (qrshl_u8_x_tied2, svuint8_t, svint8_t,
		 z0_res = svqrshl_u8_x (p0, z4, z0),
		 z0_res = svqrshl_x (p0, z4, z0))

/*
** qrshl_u8_x_untied:
** (
**	movprfx	z0, z1
**	uqrshl	z0\.b, p0/m, z0\.b, z4\.b
** |
**	movprfx	z0, z4
**	uqrshlr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_DUAL_Z (qrshl_u8_x_untied, svuint8_t, svint8_t,
	     z0 = svqrshl_u8_x (p0, z1, z4),
	     z0 = svqrshl_x (p0, z1, z4))

/*
** qrshl_w0_u8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	uqrshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u8_x_tied1, svuint8_t, int8_t,
		 z0 = svqrshl_n_u8_x (p0, z0, x0),
		 z0 = svqrshl_x (p0, z0, x0))

/*
** qrshl_w0_u8_x_untied:
**	mov	z0\.b, w0
**	uqrshlr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u8_x_untied, svuint8_t, int8_t,
		 z0 = svqrshl_n_u8_x (p0, z1, x0),
		 z0 = svqrshl_x (p0, z1, x0))

/*
** qrshl_m8_u8_x:
**	urshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (qrshl_m8_u8_x, svuint8_t,
		z0 = svqrshl_n_u8_x (p0, z0, -8),
		z0 = svqrshl_x (p0, z0, -8))

/*
** qrshl_m2_u8_x:
**	urshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_u8_x, svuint8_t,
		z0 = svqrshl_n_u8_x (p0, z0, -2),
		z0 = svqrshl_x (p0, z0, -2))

/*
** qrshl_m1_u8_x_tied1:
**	urshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u8_x_tied1, svuint8_t,
		z0 = svqrshl_n_u8_x (p0, z0, -1),
		z0 = svqrshl_x (p0, z0, -1))

/*
** qrshl_m1_u8_x_untied:
**	movprfx	z0, z1
**	urshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u8_x_untied, svuint8_t,
		z0 = svqrshl_n_u8_x (p0, z1, -1),
		z0 = svqrshl_x (p0, z1, -1))

/*
** qrshl_1_u8_x_tied1:
**	uqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u8_x_tied1, svuint8_t,
		z0 = svqrshl_n_u8_x (p0, z0, 1),
		z0 = svqrshl_x (p0, z0, 1))

/*
** qrshl_1_u8_x_untied:
**	movprfx	z0, z1
**	uqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u8_x_untied, svuint8_t,
		z0 = svqrshl_n_u8_x (p0, z1, 1),
		z0 = svqrshl_x (p0, z1, 1))

/*
** qrshl_2_u8_x:
**	uqshl	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_u8_x, svuint8_t,
		z0 = svqrshl_n_u8_x (p0, z0, 2),
		z0 = svqrshl_x (p0, z0, 2))

/*
** qrshl_7_u8_x:
**	uqshl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (qrshl_7_u8_x, svuint8_t,
		z0 = svqrshl_n_u8_x (p0, z0, 7),
		z0 = svqrshl_x (p0, z0, 7))
