/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sqadd_u16_m_tied1:
**	usqadd	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (sqadd_u16_m_tied1, svuint16_t, svint16_t,
	     z0 = svsqadd_u16_m (p0, z0, z4),
	     z0 = svsqadd_m (p0, z0, z4))

/*
** sqadd_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	usqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (sqadd_u16_m_tied2, svuint16_t, svint16_t,
		 z0_res = svsqadd_u16_m (p0, z4, z0),
		 z0_res = svsqadd_m (p0, z4, z0))

/*
** sqadd_u16_m_untied:
**	movprfx	z0, z1
**	usqadd	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (sqadd_u16_m_untied, svuint16_t, svint16_t,
	     z0 = svsqadd_u16_m (p0, z1, z4),
	     z0 = svsqadd_m (p0, z1, z4))

/*
** sqadd_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u16_m_tied1, svuint16_t, int16_t,
		 z0 = svsqadd_n_u16_m (p0, z0, x0),
		 z0 = svsqadd_m (p0, z0, x0))

/*
** sqadd_w0_u16_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u16_m_untied, svuint16_t, int16_t,
		 z0 = svsqadd_n_u16_m (p0, z1, x0),
		 z0 = svsqadd_m (p0, z1, x0))

/*
** sqadd_1_u16_m_tied1:
**	mov	(z[0-9]+\.h), #1
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u16_m_tied1, svuint16_t,
		z0 = svsqadd_n_u16_m (p0, z0, 1),
		z0 = svsqadd_m (p0, z0, 1))

/*
** sqadd_1_u16_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0, z1
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u16_m_untied, svuint16_t,
		z0 = svsqadd_n_u16_m (p0, z1, 1),
		z0 = svsqadd_m (p0, z1, 1))

/*
** sqadd_127_u16_m:
**	mov	(z[0-9]+\.h), #127
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_127_u16_m, svuint16_t,
		z0 = svsqadd_n_u16_m (p0, z0, 127),
		z0 = svsqadd_m (p0, z0, 127))

/*
** sqadd_128_u16_m:
**	mov	(z[0-9]+\.h), #128
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_128_u16_m, svuint16_t,
		z0 = svsqadd_n_u16_m (p0, z0, 128),
		z0 = svsqadd_m (p0, z0, 128))

/*
** sqadd_255_u16_m:
**	mov	(z[0-9]+\.h), #255
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_255_u16_m, svuint16_t,
		z0 = svsqadd_n_u16_m (p0, z0, 255),
		z0 = svsqadd_m (p0, z0, 255))

/*
** sqadd_m1_u16_m:
**	mov	(z[0-9]+)\.b, #-1
**	usqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (sqadd_m1_u16_m, svuint16_t,
		z0 = svsqadd_n_u16_m (p0, z0, -1),
		z0 = svsqadd_m (p0, z0, -1))

/*
** sqadd_m127_u16_m:
**	mov	(z[0-9]+\.h), #-127
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m127_u16_m, svuint16_t,
		z0 = svsqadd_n_u16_m (p0, z0, -127),
		z0 = svsqadd_m (p0, z0, -127))

/*
** sqadd_m128_u16_m:
**	mov	(z[0-9]+\.h), #-128
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m128_u16_m, svuint16_t,
		z0 = svsqadd_n_u16_m (p0, z0, -128),
		z0 = svsqadd_m (p0, z0, -128))

/*
** sqadd_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	usqadd	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (sqadd_u16_z_tied1, svuint16_t, svint16_t,
	     z0 = svsqadd_u16_z (p0, z0, z4),
	     z0 = svsqadd_z (p0, z0, z4))

/*
** sqadd_u16_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z4\.h
**	usqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (sqadd_u16_z_tied2, svuint16_t, svint16_t,
		 z0_res = svsqadd_u16_z (p0, z4, z0),
		 z0_res = svsqadd_z (p0, z4, z0))

/*
** sqadd_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	usqadd	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (sqadd_u16_z_untied, svuint16_t, svint16_t,
	     z0 = svsqadd_u16_z (p0, z1, z4),
	     z0 = svsqadd_z (p0, z1, z4))

/*
** sqadd_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u16_z_tied1, svuint16_t, int16_t,
		 z0 = svsqadd_n_u16_z (p0, z0, x0),
		 z0 = svsqadd_z (p0, z0, x0))

/*
** sqadd_w0_u16_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z1\.h
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u16_z_untied, svuint16_t, int16_t,
		 z0 = svsqadd_n_u16_z (p0, z1, x0),
		 z0 = svsqadd_z (p0, z1, x0))

/*
** sqadd_1_u16_z_tied1:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z0\.h
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u16_z_tied1, svuint16_t,
		z0 = svsqadd_n_u16_z (p0, z0, 1),
		z0 = svsqadd_z (p0, z0, 1))

/*
** sqadd_1_u16_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z1\.h
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u16_z_untied, svuint16_t,
		z0 = svsqadd_n_u16_z (p0, z1, 1),
		z0 = svsqadd_z (p0, z1, 1))

/*
** sqadd_127_u16_z:
**	mov	(z[0-9]+\.h), #127
**	movprfx	z0\.h, p0/z, z0\.h
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_127_u16_z, svuint16_t,
		z0 = svsqadd_n_u16_z (p0, z0, 127),
		z0 = svsqadd_z (p0, z0, 127))

/*
** sqadd_128_u16_z:
**	mov	(z[0-9]+\.h), #128
**	movprfx	z0\.h, p0/z, z0\.h
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_128_u16_z, svuint16_t,
		z0 = svsqadd_n_u16_z (p0, z0, 128),
		z0 = svsqadd_z (p0, z0, 128))

/*
** sqadd_255_u16_z:
**	mov	(z[0-9]+\.h), #255
**	movprfx	z0\.h, p0/z, z0\.h
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_255_u16_z, svuint16_t,
		z0 = svsqadd_n_u16_z (p0, z0, 255),
		z0 = svsqadd_z (p0, z0, 255))

/*
** sqadd_m1_u16_z:
**	mov	(z[0-9]+)\.b, #-1
**	movprfx	z0\.h, p0/z, z0\.h
**	usqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (sqadd_m1_u16_z, svuint16_t,
		z0 = svsqadd_n_u16_z (p0, z0, -1),
		z0 = svsqadd_z (p0, z0, -1))

/*
** sqadd_m127_u16_z:
**	mov	(z[0-9]+\.h), #-127
**	movprfx	z0\.h, p0/z, z0\.h
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m127_u16_z, svuint16_t,
		z0 = svsqadd_n_u16_z (p0, z0, -127),
		z0 = svsqadd_z (p0, z0, -127))

/*
** sqadd_m128_u16_z:
**	mov	(z[0-9]+\.h), #-128
**	movprfx	z0\.h, p0/z, z0\.h
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m128_u16_z, svuint16_t,
		z0 = svsqadd_n_u16_z (p0, z0, -128),
		z0 = svsqadd_z (p0, z0, -128))

/*
** sqadd_u16_x_tied1:
**	usqadd	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (sqadd_u16_x_tied1, svuint16_t, svint16_t,
	     z0 = svsqadd_u16_x (p0, z0, z4),
	     z0 = svsqadd_x (p0, z0, z4))

/*
** sqadd_u16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	usqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (sqadd_u16_x_tied2, svuint16_t, svint16_t,
		 z0_res = svsqadd_u16_x (p0, z4, z0),
		 z0_res = svsqadd_x (p0, z4, z0))

/*
** sqadd_u16_x_untied:
**	movprfx	z0, z1
**	usqadd	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (sqadd_u16_x_untied, svuint16_t, svint16_t,
	     z0 = svsqadd_u16_x (p0, z1, z4),
	     z0 = svsqadd_x (p0, z1, z4))

/*
** sqadd_w0_u16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u16_x_tied1, svuint16_t, int16_t,
		 z0 = svsqadd_n_u16_x (p0, z0, x0),
		 z0 = svsqadd_x (p0, z0, x0))

/*
** sqadd_w0_u16_x_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u16_x_untied, svuint16_t, int16_t,
		 z0 = svsqadd_n_u16_x (p0, z1, x0),
		 z0 = svsqadd_x (p0, z1, x0))

/*
** sqadd_1_u16_x_tied1:
**	uqadd	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u16_x_tied1, svuint16_t,
		z0 = svsqadd_n_u16_x (p0, z0, 1),
		z0 = svsqadd_x (p0, z0, 1))

/*
** sqadd_1_u16_x_untied:
**	movprfx	z0, z1
**	uqadd	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u16_x_untied, svuint16_t,
		z0 = svsqadd_n_u16_x (p0, z1, 1),
		z0 = svsqadd_x (p0, z1, 1))

/*
** sqadd_127_u16_x:
**	uqadd	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (sqadd_127_u16_x, svuint16_t,
		z0 = svsqadd_n_u16_x (p0, z0, 127),
		z0 = svsqadd_x (p0, z0, 127))

/*
** sqadd_128_u16_x:
**	uqadd	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (sqadd_128_u16_x, svuint16_t,
		z0 = svsqadd_n_u16_x (p0, z0, 128),
		z0 = svsqadd_x (p0, z0, 128))

/*
** sqadd_255_u16_x:
**	uqadd	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (sqadd_255_u16_x, svuint16_t,
		z0 = svsqadd_n_u16_x (p0, z0, 255),
		z0 = svsqadd_x (p0, z0, 255))

/*
** sqadd_m1_u16_x:
**	mov	(z[0-9]+)\.b, #-1
**	usqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (sqadd_m1_u16_x, svuint16_t,
		z0 = svsqadd_n_u16_x (p0, z0, -1),
		z0 = svsqadd_x (p0, z0, -1))

/*
** sqadd_m127_u16_x:
**	mov	(z[0-9]+\.h), #-127
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m127_u16_x, svuint16_t,
		z0 = svsqadd_n_u16_x (p0, z0, -127),
		z0 = svsqadd_x (p0, z0, -127))

/*
** sqadd_m128_u16_x:
**	mov	(z[0-9]+\.h), #-128
**	usqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m128_u16_x, svuint16_t,
		z0 = svsqadd_n_u16_x (p0, z0, -128),
		z0 = svsqadd_x (p0, z0, -128))
