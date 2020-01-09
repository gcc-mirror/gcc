/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sqadd_u32_m_tied1:
**	usqadd	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (sqadd_u32_m_tied1, svuint32_t, svint32_t,
	     z0 = svsqadd_u32_m (p0, z0, z4),
	     z0 = svsqadd_m (p0, z0, z4))

/*
** sqadd_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	usqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (sqadd_u32_m_tied2, svuint32_t, svint32_t,
		 z0_res = svsqadd_u32_m (p0, z4, z0),
		 z0_res = svsqadd_m (p0, z4, z0))

/*
** sqadd_u32_m_untied:
**	movprfx	z0, z1
**	usqadd	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (sqadd_u32_m_untied, svuint32_t, svint32_t,
	     z0 = svsqadd_u32_m (p0, z1, z4),
	     z0 = svsqadd_m (p0, z1, z4))

/*
** sqadd_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u32_m_tied1, svuint32_t, int32_t,
		 z0 = svsqadd_n_u32_m (p0, z0, x0),
		 z0 = svsqadd_m (p0, z0, x0))

/*
** sqadd_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u32_m_untied, svuint32_t, int32_t,
		 z0 = svsqadd_n_u32_m (p0, z1, x0),
		 z0 = svsqadd_m (p0, z1, x0))

/*
** sqadd_1_u32_m_tied1:
**	mov	(z[0-9]+\.s), #1
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u32_m_tied1, svuint32_t,
		z0 = svsqadd_n_u32_m (p0, z0, 1),
		z0 = svsqadd_m (p0, z0, 1))

/*
** sqadd_1_u32_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0, z1
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u32_m_untied, svuint32_t,
		z0 = svsqadd_n_u32_m (p0, z1, 1),
		z0 = svsqadd_m (p0, z1, 1))

/*
** sqadd_127_u32_m:
**	mov	(z[0-9]+\.s), #127
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_127_u32_m, svuint32_t,
		z0 = svsqadd_n_u32_m (p0, z0, 127),
		z0 = svsqadd_m (p0, z0, 127))

/*
** sqadd_128_u32_m:
**	mov	(z[0-9]+\.s), #128
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_128_u32_m, svuint32_t,
		z0 = svsqadd_n_u32_m (p0, z0, 128),
		z0 = svsqadd_m (p0, z0, 128))

/*
** sqadd_255_u32_m:
**	mov	(z[0-9]+\.s), #255
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_255_u32_m, svuint32_t,
		z0 = svsqadd_n_u32_m (p0, z0, 255),
		z0 = svsqadd_m (p0, z0, 255))

/*
** sqadd_m1_u32_m:
**	mov	(z[0-9]+)\.b, #-1
**	usqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (sqadd_m1_u32_m, svuint32_t,
		z0 = svsqadd_n_u32_m (p0, z0, -1),
		z0 = svsqadd_m (p0, z0, -1))

/*
** sqadd_m127_u32_m:
**	mov	(z[0-9]+\.s), #-127
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m127_u32_m, svuint32_t,
		z0 = svsqadd_n_u32_m (p0, z0, -127),
		z0 = svsqadd_m (p0, z0, -127))

/*
** sqadd_m128_u32_m:
**	mov	(z[0-9]+\.s), #-128
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m128_u32_m, svuint32_t,
		z0 = svsqadd_n_u32_m (p0, z0, -128),
		z0 = svsqadd_m (p0, z0, -128))

/*
** sqadd_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	usqadd	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (sqadd_u32_z_tied1, svuint32_t, svint32_t,
	     z0 = svsqadd_u32_z (p0, z0, z4),
	     z0 = svsqadd_z (p0, z0, z4))

/*
** sqadd_u32_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, z4\.s
**	usqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (sqadd_u32_z_tied2, svuint32_t, svint32_t,
		 z0_res = svsqadd_u32_z (p0, z4, z0),
		 z0_res = svsqadd_z (p0, z4, z0))

/*
** sqadd_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	usqadd	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (sqadd_u32_z_untied, svuint32_t, svint32_t,
	     z0 = svsqadd_u32_z (p0, z1, z4),
	     z0 = svsqadd_z (p0, z1, z4))

/*
** sqadd_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u32_z_tied1, svuint32_t, int32_t,
		 z0 = svsqadd_n_u32_z (p0, z0, x0),
		 z0 = svsqadd_z (p0, z0, x0))

/*
** sqadd_w0_u32_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z1\.s
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u32_z_untied, svuint32_t, int32_t,
		 z0 = svsqadd_n_u32_z (p0, z1, x0),
		 z0 = svsqadd_z (p0, z1, x0))

/*
** sqadd_1_u32_z_tied1:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z0\.s
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u32_z_tied1, svuint32_t,
		z0 = svsqadd_n_u32_z (p0, z0, 1),
		z0 = svsqadd_z (p0, z0, 1))

/*
** sqadd_1_u32_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z1\.s
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u32_z_untied, svuint32_t,
		z0 = svsqadd_n_u32_z (p0, z1, 1),
		z0 = svsqadd_z (p0, z1, 1))

/*
** sqadd_127_u32_z:
**	mov	(z[0-9]+\.s), #127
**	movprfx	z0\.s, p0/z, z0\.s
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_127_u32_z, svuint32_t,
		z0 = svsqadd_n_u32_z (p0, z0, 127),
		z0 = svsqadd_z (p0, z0, 127))

/*
** sqadd_128_u32_z:
**	mov	(z[0-9]+\.s), #128
**	movprfx	z0\.s, p0/z, z0\.s
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_128_u32_z, svuint32_t,
		z0 = svsqadd_n_u32_z (p0, z0, 128),
		z0 = svsqadd_z (p0, z0, 128))

/*
** sqadd_255_u32_z:
**	mov	(z[0-9]+\.s), #255
**	movprfx	z0\.s, p0/z, z0\.s
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_255_u32_z, svuint32_t,
		z0 = svsqadd_n_u32_z (p0, z0, 255),
		z0 = svsqadd_z (p0, z0, 255))

/*
** sqadd_m1_u32_z:
**	mov	(z[0-9]+)\.b, #-1
**	movprfx	z0\.s, p0/z, z0\.s
**	usqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (sqadd_m1_u32_z, svuint32_t,
		z0 = svsqadd_n_u32_z (p0, z0, -1),
		z0 = svsqadd_z (p0, z0, -1))

/*
** sqadd_m127_u32_z:
**	mov	(z[0-9]+\.s), #-127
**	movprfx	z0\.s, p0/z, z0\.s
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m127_u32_z, svuint32_t,
		z0 = svsqadd_n_u32_z (p0, z0, -127),
		z0 = svsqadd_z (p0, z0, -127))

/*
** sqadd_m128_u32_z:
**	mov	(z[0-9]+\.s), #-128
**	movprfx	z0\.s, p0/z, z0\.s
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m128_u32_z, svuint32_t,
		z0 = svsqadd_n_u32_z (p0, z0, -128),
		z0 = svsqadd_z (p0, z0, -128))

/*
** sqadd_u32_x_tied1:
**	usqadd	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (sqadd_u32_x_tied1, svuint32_t, svint32_t,
	     z0 = svsqadd_u32_x (p0, z0, z4),
	     z0 = svsqadd_x (p0, z0, z4))

/*
** sqadd_u32_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	usqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (sqadd_u32_x_tied2, svuint32_t, svint32_t,
		 z0_res = svsqadd_u32_x (p0, z4, z0),
		 z0_res = svsqadd_x (p0, z4, z0))

/*
** sqadd_u32_x_untied:
**	movprfx	z0, z1
**	usqadd	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (sqadd_u32_x_untied, svuint32_t, svint32_t,
	     z0 = svsqadd_u32_x (p0, z1, z4),
	     z0 = svsqadd_x (p0, z1, z4))

/*
** sqadd_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u32_x_tied1, svuint32_t, int32_t,
		 z0 = svsqadd_n_u32_x (p0, z0, x0),
		 z0 = svsqadd_x (p0, z0, x0))

/*
** sqadd_w0_u32_x_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_w0_u32_x_untied, svuint32_t, int32_t,
		 z0 = svsqadd_n_u32_x (p0, z1, x0),
		 z0 = svsqadd_x (p0, z1, x0))

/*
** sqadd_1_u32_x_tied1:
**	uqadd	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u32_x_tied1, svuint32_t,
		z0 = svsqadd_n_u32_x (p0, z0, 1),
		z0 = svsqadd_x (p0, z0, 1))

/*
** sqadd_1_u32_x_untied:
**	movprfx	z0, z1
**	uqadd	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u32_x_untied, svuint32_t,
		z0 = svsqadd_n_u32_x (p0, z1, 1),
		z0 = svsqadd_x (p0, z1, 1))

/*
** sqadd_127_u32_x:
**	uqadd	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (sqadd_127_u32_x, svuint32_t,
		z0 = svsqadd_n_u32_x (p0, z0, 127),
		z0 = svsqadd_x (p0, z0, 127))

/*
** sqadd_128_u32_x:
**	uqadd	z0\.s, z0\.s, #128
**	ret
*/
TEST_UNIFORM_Z (sqadd_128_u32_x, svuint32_t,
		z0 = svsqadd_n_u32_x (p0, z0, 128),
		z0 = svsqadd_x (p0, z0, 128))

/*
** sqadd_255_u32_x:
**	uqadd	z0\.s, z0\.s, #255
**	ret
*/
TEST_UNIFORM_Z (sqadd_255_u32_x, svuint32_t,
		z0 = svsqadd_n_u32_x (p0, z0, 255),
		z0 = svsqadd_x (p0, z0, 255))

/*
** sqadd_m1_u32_x:
**	mov	(z[0-9]+)\.b, #-1
**	usqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (sqadd_m1_u32_x, svuint32_t,
		z0 = svsqadd_n_u32_x (p0, z0, -1),
		z0 = svsqadd_x (p0, z0, -1))

/*
** sqadd_m127_u32_x:
**	mov	(z[0-9]+\.s), #-127
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m127_u32_x, svuint32_t,
		z0 = svsqadd_n_u32_x (p0, z0, -127),
		z0 = svsqadd_x (p0, z0, -127))

/*
** sqadd_m128_u32_x:
**	mov	(z[0-9]+\.s), #-128
**	usqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m128_u32_x, svuint32_t,
		z0 = svsqadd_n_u32_x (p0, z0, -128),
		z0 = svsqadd_x (p0, z0, -128))
