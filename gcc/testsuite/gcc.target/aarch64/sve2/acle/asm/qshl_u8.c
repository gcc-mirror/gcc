/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshl_u8_m_tied1:
**	uqshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (qshl_u8_m_tied1, svuint8_t, svint8_t,
	     z0 = svqshl_u8_m (p0, z0, z4),
	     z0 = svqshl_m (p0, z0, z4))

/*
** qshl_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uqshl	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (qshl_u8_m_tied2, svuint8_t, svint8_t,
		 z0_res = svqshl_u8_m (p0, z4, z0),
		 z0_res = svqshl_m (p0, z4, z0))

/*
** qshl_u8_m_untied:
**	movprfx	z0, z1
**	uqshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (qshl_u8_m_untied, svuint8_t, svint8_t,
	     z0 = svqshl_u8_m (p0, z1, z4),
	     z0 = svqshl_m (p0, z1, z4))

/*
** qshl_w0_u8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	uqshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u8_m_tied1, svuint8_t, int8_t,
		 z0 = svqshl_n_u8_m (p0, z0, x0),
		 z0 = svqshl_m (p0, z0, x0))

/*
** qshl_w0_u8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	uqshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u8_m_untied, svuint8_t, int8_t,
		 z0 = svqshl_n_u8_m (p0, z1, x0),
		 z0 = svqshl_m (p0, z1, x0))

/*
** qshl_m8_u8_m:
**	lsr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (qshl_m8_u8_m, svuint8_t,
		z0 = svqshl_n_u8_m (p0, z0, -8),
		z0 = svqshl_m (p0, z0, -8))

/*
** qshl_m2_u8_m:
**	lsr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_u8_m, svuint8_t,
		z0 = svqshl_n_u8_m (p0, z0, -2),
		z0 = svqshl_m (p0, z0, -2))

/*
** qshl_m1_u8_m_tied1:
**	lsr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u8_m_tied1, svuint8_t,
		z0 = svqshl_n_u8_m (p0, z0, -1),
		z0 = svqshl_m (p0, z0, -1))

/*
** qshl_m1_u8_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u8_m_untied, svuint8_t,
		z0 = svqshl_n_u8_m (p0, z1, -1),
		z0 = svqshl_m (p0, z1, -1))

/*
** qshl_1_u8_m_tied1:
**	uqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u8_m_tied1, svuint8_t,
		z0 = svqshl_n_u8_m (p0, z0, 1),
		z0 = svqshl_m (p0, z0, 1))

/*
** qshl_1_u8_m_untied:
**	movprfx	z0, z1
**	uqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u8_m_untied, svuint8_t,
		z0 = svqshl_n_u8_m (p0, z1, 1),
		z0 = svqshl_m (p0, z1, 1))

/*
** qshl_2_u8_m:
**	uqshl	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_u8_m, svuint8_t,
		z0 = svqshl_n_u8_m (p0, z0, 2),
		z0 = svqshl_m (p0, z0, 2))

/*
** qshl_7_u8_m:
**	uqshl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (qshl_7_u8_m, svuint8_t,
		z0 = svqshl_n_u8_m (p0, z0, 7),
		z0 = svqshl_m (p0, z0, 7))

/*
** qshl_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	uqshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (qshl_u8_z_tied1, svuint8_t, svint8_t,
	     z0 = svqshl_u8_z (p0, z0, z4),
	     z0 = svqshl_z (p0, z0, z4))

/*
** qshl_u8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	uqshlr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z_REV (qshl_u8_z_tied2, svuint8_t, svint8_t,
		 z0_res = svqshl_u8_z (p0, z4, z0),
		 z0_res = svqshl_z (p0, z4, z0))

/*
** qshl_u8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uqshl	z0\.b, p0/m, z0\.b, z4\.b
** |
**	movprfx	z0\.b, p0/z, z4\.b
**	uqshlr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_DUAL_Z (qshl_u8_z_untied, svuint8_t, svint8_t,
	     z0 = svqshl_u8_z (p0, z1, z4),
	     z0 = svqshl_z (p0, z1, z4))

/*
** qshl_w0_u8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	uqshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u8_z_tied1, svuint8_t, int8_t,
		 z0 = svqshl_n_u8_z (p0, z0, x0),
		 z0 = svqshl_z (p0, z0, x0))

/*
** qshl_w0_u8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uqshl	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	uqshlr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u8_z_untied, svuint8_t, int8_t,
		 z0 = svqshl_n_u8_z (p0, z1, x0),
		 z0 = svqshl_z (p0, z1, x0))

/*
** qshl_m8_u8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (qshl_m8_u8_z, svuint8_t,
		z0 = svqshl_n_u8_z (p0, z0, -8),
		z0 = svqshl_z (p0, z0, -8))

/*
** qshl_m2_u8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_u8_z, svuint8_t,
		z0 = svqshl_n_u8_z (p0, z0, -2),
		z0 = svqshl_z (p0, z0, -2))

/*
** qshl_m1_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u8_z_tied1, svuint8_t,
		z0 = svqshl_n_u8_z (p0, z0, -1),
		z0 = svqshl_z (p0, z0, -1))

/*
** qshl_m1_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	lsr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u8_z_untied, svuint8_t,
		z0 = svqshl_n_u8_z (p0, z1, -1),
		z0 = svqshl_z (p0, z1, -1))

/*
** qshl_1_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	uqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u8_z_tied1, svuint8_t,
		z0 = svqshl_n_u8_z (p0, z0, 1),
		z0 = svqshl_z (p0, z0, 1))

/*
** qshl_1_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	uqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u8_z_untied, svuint8_t,
		z0 = svqshl_n_u8_z (p0, z1, 1),
		z0 = svqshl_z (p0, z1, 1))

/*
** qshl_2_u8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	uqshl	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_u8_z, svuint8_t,
		z0 = svqshl_n_u8_z (p0, z0, 2),
		z0 = svqshl_z (p0, z0, 2))

/*
** qshl_7_u8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	uqshl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (qshl_7_u8_z, svuint8_t,
		z0 = svqshl_n_u8_z (p0, z0, 7),
		z0 = svqshl_z (p0, z0, 7))

/*
** qshl_u8_x_tied1:
**	uqshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (qshl_u8_x_tied1, svuint8_t, svint8_t,
	     z0 = svqshl_u8_x (p0, z0, z4),
	     z0 = svqshl_x (p0, z0, z4))

/*
** qshl_u8_x_tied2:
**	uqshlr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z_REV (qshl_u8_x_tied2, svuint8_t, svint8_t,
		 z0_res = svqshl_u8_x (p0, z4, z0),
		 z0_res = svqshl_x (p0, z4, z0))

/*
** qshl_u8_x_untied:
** (
**	movprfx	z0, z1
**	uqshl	z0\.b, p0/m, z0\.b, z4\.b
** |
**	movprfx	z0, z4
**	uqshlr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_DUAL_Z (qshl_u8_x_untied, svuint8_t, svint8_t,
	     z0 = svqshl_u8_x (p0, z1, z4),
	     z0 = svqshl_x (p0, z1, z4))

/*
** qshl_w0_u8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	uqshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u8_x_tied1, svuint8_t, int8_t,
		 z0 = svqshl_n_u8_x (p0, z0, x0),
		 z0 = svqshl_x (p0, z0, x0))

/*
** qshl_w0_u8_x_untied:
**	mov	z0\.b, w0
**	uqshlr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u8_x_untied, svuint8_t, int8_t,
		 z0 = svqshl_n_u8_x (p0, z1, x0),
		 z0 = svqshl_x (p0, z1, x0))

/*
** qshl_m8_u8_x:
**	lsr	z0\.b, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (qshl_m8_u8_x, svuint8_t,
		z0 = svqshl_n_u8_x (p0, z0, -8),
		z0 = svqshl_x (p0, z0, -8))

/*
** qshl_m2_u8_x:
**	lsr	z0\.b, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_u8_x, svuint8_t,
		z0 = svqshl_n_u8_x (p0, z0, -2),
		z0 = svqshl_x (p0, z0, -2))

/*
** qshl_m1_u8_x_tied1:
**	lsr	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u8_x_tied1, svuint8_t,
		z0 = svqshl_n_u8_x (p0, z0, -1),
		z0 = svqshl_x (p0, z0, -1))

/*
** qshl_m1_u8_x_untied:
**	lsr	z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u8_x_untied, svuint8_t,
		z0 = svqshl_n_u8_x (p0, z1, -1),
		z0 = svqshl_x (p0, z1, -1))

/*
** qshl_1_u8_x_tied1:
**	uqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u8_x_tied1, svuint8_t,
		z0 = svqshl_n_u8_x (p0, z0, 1),
		z0 = svqshl_x (p0, z0, 1))

/*
** qshl_1_u8_x_untied:
**	movprfx	z0, z1
**	uqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u8_x_untied, svuint8_t,
		z0 = svqshl_n_u8_x (p0, z1, 1),
		z0 = svqshl_x (p0, z1, 1))

/*
** qshl_2_u8_x:
**	uqshl	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_u8_x, svuint8_t,
		z0 = svqshl_n_u8_x (p0, z0, 2),
		z0 = svqshl_x (p0, z0, 2))

/*
** qshl_7_u8_x:
**	uqshl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (qshl_7_u8_x, svuint8_t,
		z0 = svqshl_n_u8_x (p0, z0, 7),
		z0 = svqshl_x (p0, z0, 7))
