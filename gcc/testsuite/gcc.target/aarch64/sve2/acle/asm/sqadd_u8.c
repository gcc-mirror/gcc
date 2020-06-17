/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sqadd_u8_m_tied1:
**	usqadd	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (sqadd_u8_m_tied1, svuint8_t, svint8_t,
	     z0 = svsqadd_u8_m (p0, z0, z4),
	     z0 = svsqadd_m (p0, z0, z4))

/*
** sqadd_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	usqadd	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (sqadd_u8_m_tied2, svuint8_t, svint8_t,
		 z0_res = svsqadd_u8_m (p0, z4, z0),
		 z0_res = svsqadd_m (p0, z4, z0))

/*
** sqadd_u8_m_untied:
**	movprfx	z0, z1
**	usqadd	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (sqadd_u8_m_untied, svuint8_t, svint8_t,
	     z0 = svsqadd_u8_m (p0, z1, z4),
	     z0 = svsqadd_m (p0, z1, z4))

/*
** sqadd_w0_u8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u8_m_tied1, svuint8_t, int8_t,
		 z0 = svsqadd_n_u8_m (p0, z0, x0),
		 z0 = svsqadd_m (p0, z0, x0))

/*
** sqadd_w0_u8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u8_m_untied, svuint8_t, int8_t,
		 z0 = svsqadd_n_u8_m (p0, z1, x0),
		 z0 = svsqadd_m (p0, z1, x0))

/*
** sqadd_1_u8_m_tied1:
**	mov	(z[0-9]+\.b), #1
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u8_m_tied1, svuint8_t,
		z0 = svsqadd_n_u8_m (p0, z0, 1),
		z0 = svsqadd_m (p0, z0, 1))

/*
** sqadd_1_u8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0, z1
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u8_m_untied, svuint8_t,
		z0 = svsqadd_n_u8_m (p0, z1, 1),
		z0 = svsqadd_m (p0, z1, 1))

/*
** sqadd_127_u8_m:
**	mov	(z[0-9]+\.b), #127
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_127_u8_m, svuint8_t,
		z0 = svsqadd_n_u8_m (p0, z0, 127),
		z0 = svsqadd_m (p0, z0, 127))

/*
** sqadd_128_u8_m:
**	mov	(z[0-9]+\.b), #-128
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_128_u8_m, svuint8_t,
		z0 = svsqadd_n_u8_m (p0, z0, 128),
		z0 = svsqadd_m (p0, z0, 128))

/*
** sqadd_255_u8_m:
**	mov	(z[0-9]+\.b), #-1
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_255_u8_m, svuint8_t,
		z0 = svsqadd_n_u8_m (p0, z0, 255),
		z0 = svsqadd_m (p0, z0, 255))

/*
** sqadd_m1_u8_m:
**	mov	(z[0-9]+\.b), #-1
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m1_u8_m, svuint8_t,
		z0 = svsqadd_n_u8_m (p0, z0, -1),
		z0 = svsqadd_m (p0, z0, -1))

/*
** sqadd_m127_u8_m:
**	mov	(z[0-9]+\.b), #-127
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m127_u8_m, svuint8_t,
		z0 = svsqadd_n_u8_m (p0, z0, -127),
		z0 = svsqadd_m (p0, z0, -127))

/*
** sqadd_m128_u8_m:
**	mov	(z[0-9]+\.b), #-128
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m128_u8_m, svuint8_t,
		z0 = svsqadd_n_u8_m (p0, z0, -128),
		z0 = svsqadd_m (p0, z0, -128))

/*
** sqadd_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	usqadd	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (sqadd_u8_z_tied1, svuint8_t, svint8_t,
	     z0 = svsqadd_u8_z (p0, z0, z4),
	     z0 = svsqadd_z (p0, z0, z4))

/*
** sqadd_u8_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.b, p0/z, z4\.b
**	usqadd	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (sqadd_u8_z_tied2, svuint8_t, svint8_t,
		 z0_res = svsqadd_u8_z (p0, z4, z0),
		 z0_res = svsqadd_z (p0, z4, z0))

/*
** sqadd_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	usqadd	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (sqadd_u8_z_untied, svuint8_t, svint8_t,
	     z0 = svsqadd_u8_z (p0, z1, z4),
	     z0 = svsqadd_z (p0, z1, z4))

/*
** sqadd_w0_u8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u8_z_tied1, svuint8_t, int8_t,
		 z0 = svsqadd_n_u8_z (p0, z0, x0),
		 z0 = svsqadd_z (p0, z0, x0))

/*
** sqadd_w0_u8_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z1\.b
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u8_z_untied, svuint8_t, int8_t,
		 z0 = svsqadd_n_u8_z (p0, z1, x0),
		 z0 = svsqadd_z (p0, z1, x0))

/*
** sqadd_1_u8_z_tied1:
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0\.b, p0/z, z0\.b
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u8_z_tied1, svuint8_t,
		z0 = svsqadd_n_u8_z (p0, z0, 1),
		z0 = svsqadd_z (p0, z0, 1))

/*
** sqadd_1_u8_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0\.b, p0/z, z1\.b
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u8_z_untied, svuint8_t,
		z0 = svsqadd_n_u8_z (p0, z1, 1),
		z0 = svsqadd_z (p0, z1, 1))

/*
** sqadd_127_u8_z:
**	mov	(z[0-9]+\.b), #127
**	movprfx	z0\.b, p0/z, z0\.b
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_127_u8_z, svuint8_t,
		z0 = svsqadd_n_u8_z (p0, z0, 127),
		z0 = svsqadd_z (p0, z0, 127))

/*
** sqadd_128_u8_z:
**	mov	(z[0-9]+\.b), #-128
**	movprfx	z0\.b, p0/z, z0\.b
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_128_u8_z, svuint8_t,
		z0 = svsqadd_n_u8_z (p0, z0, 128),
		z0 = svsqadd_z (p0, z0, 128))

/*
** sqadd_255_u8_z:
**	mov	(z[0-9]+\.b), #-1
**	movprfx	z0\.b, p0/z, z0\.b
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_255_u8_z, svuint8_t,
		z0 = svsqadd_n_u8_z (p0, z0, 255),
		z0 = svsqadd_z (p0, z0, 255))

/*
** sqadd_m1_u8_z:
**	mov	(z[0-9]+\.b), #-1
**	movprfx	z0\.b, p0/z, z0\.b
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m1_u8_z, svuint8_t,
		z0 = svsqadd_n_u8_z (p0, z0, -1),
		z0 = svsqadd_z (p0, z0, -1))

/*
** sqadd_m127_u8_z:
**	mov	(z[0-9]+\.b), #-127
**	movprfx	z0\.b, p0/z, z0\.b
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m127_u8_z, svuint8_t,
		z0 = svsqadd_n_u8_z (p0, z0, -127),
		z0 = svsqadd_z (p0, z0, -127))

/*
** sqadd_m128_u8_z:
**	mov	(z[0-9]+\.b), #-128
**	movprfx	z0\.b, p0/z, z0\.b
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m128_u8_z, svuint8_t,
		z0 = svsqadd_n_u8_z (p0, z0, -128),
		z0 = svsqadd_z (p0, z0, -128))

/*
** sqadd_u8_x_tied1:
**	usqadd	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (sqadd_u8_x_tied1, svuint8_t, svint8_t,
	     z0 = svsqadd_u8_x (p0, z0, z4),
	     z0 = svsqadd_x (p0, z0, z4))

/*
** sqadd_u8_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	usqadd	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (sqadd_u8_x_tied2, svuint8_t, svint8_t,
		 z0_res = svsqadd_u8_x (p0, z4, z0),
		 z0_res = svsqadd_x (p0, z4, z0))

/*
** sqadd_u8_x_untied:
**	movprfx	z0, z1
**	usqadd	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (sqadd_u8_x_untied, svuint8_t, svint8_t,
	     z0 = svsqadd_u8_x (p0, z1, z4),
	     z0 = svsqadd_x (p0, z1, z4))

/*
** sqadd_w0_u8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u8_x_tied1, svuint8_t, int8_t,
		 z0 = svsqadd_n_u8_x (p0, z0, x0),
		 z0 = svsqadd_x (p0, z0, x0))

/*
** sqadd_w0_u8_x_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u8_x_untied, svuint8_t, int8_t,
		 z0 = svsqadd_n_u8_x (p0, z1, x0),
		 z0 = svsqadd_x (p0, z1, x0))

/*
** sqadd_1_u8_x_tied1:
**	uqadd	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u8_x_tied1, svuint8_t,
		z0 = svsqadd_n_u8_x (p0, z0, 1),
		z0 = svsqadd_x (p0, z0, 1))

/*
** sqadd_1_u8_x_untied:
**	movprfx	z0, z1
**	uqadd	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u8_x_untied, svuint8_t,
		z0 = svsqadd_n_u8_x (p0, z1, 1),
		z0 = svsqadd_x (p0, z1, 1))

/*
** sqadd_127_u8_x:
**	uqadd	z0\.b, z0\.b, #127
**	ret
*/
TEST_UNIFORM_Z (sqadd_127_u8_x, svuint8_t,
		z0 = svsqadd_n_u8_x (p0, z0, 127),
		z0 = svsqadd_x (p0, z0, 127))

/*
** sqadd_128_u8_x:
**	mov	(z[0-9]+\.b), #-128
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_128_u8_x, svuint8_t,
		z0 = svsqadd_n_u8_x (p0, z0, 128),
		z0 = svsqadd_x (p0, z0, 128))

/*
** sqadd_255_u8_x:
**	mov	(z[0-9]+\.b), #-1
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_255_u8_x, svuint8_t,
		z0 = svsqadd_n_u8_x (p0, z0, 255),
		z0 = svsqadd_x (p0, z0, 255))

/*
** sqadd_m1_u8_x:
**	mov	(z[0-9]+\.b), #-1
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m1_u8_x, svuint8_t,
		z0 = svsqadd_n_u8_x (p0, z0, -1),
		z0 = svsqadd_x (p0, z0, -1))

/*
** sqadd_m127_u8_x:
**	mov	(z[0-9]+\.b), #-127
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m127_u8_x, svuint8_t,
		z0 = svsqadd_n_u8_x (p0, z0, -127),
		z0 = svsqadd_x (p0, z0, -127))

/*
** sqadd_m128_u8_x:
**	mov	(z[0-9]+\.b), #-128
**	usqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m128_u8_x, svuint8_t,
		z0 = svsqadd_n_u8_x (p0, z0, -128),
		z0 = svsqadd_x (p0, z0, -128))
