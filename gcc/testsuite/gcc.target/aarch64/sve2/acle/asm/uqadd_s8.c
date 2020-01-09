/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uqadd_s8_m_tied1:
**	suqadd	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (uqadd_s8_m_tied1, svint8_t, svuint8_t,
	     z0 = svuqadd_s8_m (p0, z0, z4),
	     z0 = svuqadd_m (p0, z0, z4))

/*
** uqadd_s8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	suqadd	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (uqadd_s8_m_tied2, svint8_t, svuint8_t,
		 z0_res = svuqadd_s8_m (p0, z4, z0),
		 z0_res = svuqadd_m (p0, z4, z0))

/*
** uqadd_s8_m_untied:
**	movprfx	z0, z1
**	suqadd	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (uqadd_s8_m_untied, svint8_t, svuint8_t,
	     z0 = svuqadd_s8_m (p0, z1, z4),
	     z0 = svuqadd_m (p0, z1, z4))

/*
** uqadd_w0_s8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s8_m_tied1, svint8_t, uint8_t,
		 z0 = svuqadd_n_s8_m (p0, z0, x0),
		 z0 = svuqadd_m (p0, z0, x0))

/*
** uqadd_w0_s8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s8_m_untied, svint8_t, uint8_t,
		 z0 = svuqadd_n_s8_m (p0, z1, x0),
		 z0 = svuqadd_m (p0, z1, x0))

/*
** uqadd_1_s8_m_tied1:
**	mov	(z[0-9]+\.b), #1
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s8_m_tied1, svint8_t,
		z0 = svuqadd_n_s8_m (p0, z0, 1),
		z0 = svuqadd_m (p0, z0, 1))

/*
** uqadd_1_s8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0, z1
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s8_m_untied, svint8_t,
		z0 = svuqadd_n_s8_m (p0, z1, 1),
		z0 = svuqadd_m (p0, z1, 1))

/*
** uqadd_127_s8_m:
**	mov	(z[0-9]+\.b), #127
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_127_s8_m, svint8_t,
		z0 = svuqadd_n_s8_m (p0, z0, 127),
		z0 = svuqadd_m (p0, z0, 127))

/*
** uqadd_128_s8_m:
**	mov	(z[0-9]+\.b), #-128
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_128_s8_m, svint8_t,
		z0 = svuqadd_n_s8_m (p0, z0, 128),
		z0 = svuqadd_m (p0, z0, 128))

/*
** uqadd_255_s8_m:
**	mov	(z[0-9]+\.b), #-1
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_255_s8_m, svint8_t,
		z0 = svuqadd_n_s8_m (p0, z0, 255),
		z0 = svuqadd_m (p0, z0, 255))

/*
** uqadd_m1_s8_m:
**	mov	(z[0-9]+\.b), #-1
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m1_s8_m, svint8_t,
		z0 = svuqadd_n_s8_m (p0, z0, -1),
		z0 = svuqadd_m (p0, z0, -1))

/*
** uqadd_m127_s8_m:
**	mov	(z[0-9]+\.b), #-127
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m127_s8_m, svint8_t,
		z0 = svuqadd_n_s8_m (p0, z0, -127),
		z0 = svuqadd_m (p0, z0, -127))

/*
** uqadd_m128_s8_m:
**	mov	(z[0-9]+\.b), #-128
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m128_s8_m, svint8_t,
		z0 = svuqadd_n_s8_m (p0, z0, -128),
		z0 = svuqadd_m (p0, z0, -128))

/*
** uqadd_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	suqadd	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (uqadd_s8_z_tied1, svint8_t, svuint8_t,
	     z0 = svuqadd_s8_z (p0, z0, z4),
	     z0 = svuqadd_z (p0, z0, z4))

/*
** uqadd_s8_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.b, p0/z, z4\.b
**	suqadd	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (uqadd_s8_z_tied2, svint8_t, svuint8_t,
		 z0_res = svuqadd_s8_z (p0, z4, z0),
		 z0_res = svuqadd_z (p0, z4, z0))

/*
** uqadd_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	suqadd	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (uqadd_s8_z_untied, svint8_t, svuint8_t,
	     z0 = svuqadd_s8_z (p0, z1, z4),
	     z0 = svuqadd_z (p0, z1, z4))

/*
** uqadd_w0_s8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s8_z_tied1, svint8_t, uint8_t,
		 z0 = svuqadd_n_s8_z (p0, z0, x0),
		 z0 = svuqadd_z (p0, z0, x0))

/*
** uqadd_w0_s8_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z1\.b
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s8_z_untied, svint8_t, uint8_t,
		 z0 = svuqadd_n_s8_z (p0, z1, x0),
		 z0 = svuqadd_z (p0, z1, x0))

/*
** uqadd_1_s8_z_tied1:
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0\.b, p0/z, z0\.b
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s8_z_tied1, svint8_t,
		z0 = svuqadd_n_s8_z (p0, z0, 1),
		z0 = svuqadd_z (p0, z0, 1))

/*
** uqadd_1_s8_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0\.b, p0/z, z1\.b
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s8_z_untied, svint8_t,
		z0 = svuqadd_n_s8_z (p0, z1, 1),
		z0 = svuqadd_z (p0, z1, 1))

/*
** uqadd_127_s8_z:
**	mov	(z[0-9]+\.b), #127
**	movprfx	z0\.b, p0/z, z0\.b
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_127_s8_z, svint8_t,
		z0 = svuqadd_n_s8_z (p0, z0, 127),
		z0 = svuqadd_z (p0, z0, 127))

/*
** uqadd_128_s8_z:
**	mov	(z[0-9]+\.b), #-128
**	movprfx	z0\.b, p0/z, z0\.b
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_128_s8_z, svint8_t,
		z0 = svuqadd_n_s8_z (p0, z0, 128),
		z0 = svuqadd_z (p0, z0, 128))

/*
** uqadd_255_s8_z:
**	mov	(z[0-9]+\.b), #-1
**	movprfx	z0\.b, p0/z, z0\.b
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_255_s8_z, svint8_t,
		z0 = svuqadd_n_s8_z (p0, z0, 255),
		z0 = svuqadd_z (p0, z0, 255))

/*
** uqadd_m1_s8_z:
**	mov	(z[0-9]+\.b), #-1
**	movprfx	z0\.b, p0/z, z0\.b
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m1_s8_z, svint8_t,
		z0 = svuqadd_n_s8_z (p0, z0, -1),
		z0 = svuqadd_z (p0, z0, -1))

/*
** uqadd_m127_s8_z:
**	mov	(z[0-9]+\.b), #-127
**	movprfx	z0\.b, p0/z, z0\.b
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m127_s8_z, svint8_t,
		z0 = svuqadd_n_s8_z (p0, z0, -127),
		z0 = svuqadd_z (p0, z0, -127))

/*
** uqadd_m128_s8_z:
**	mov	(z[0-9]+\.b), #-128
**	movprfx	z0\.b, p0/z, z0\.b
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m128_s8_z, svint8_t,
		z0 = svuqadd_n_s8_z (p0, z0, -128),
		z0 = svuqadd_z (p0, z0, -128))

/*
** uqadd_s8_x_tied1:
**	suqadd	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (uqadd_s8_x_tied1, svint8_t, svuint8_t,
	     z0 = svuqadd_s8_x (p0, z0, z4),
	     z0 = svuqadd_x (p0, z0, z4))

/*
** uqadd_s8_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	suqadd	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (uqadd_s8_x_tied2, svint8_t, svuint8_t,
		 z0_res = svuqadd_s8_x (p0, z4, z0),
		 z0_res = svuqadd_x (p0, z4, z0))

/*
** uqadd_s8_x_untied:
**	movprfx	z0, z1
**	suqadd	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (uqadd_s8_x_untied, svint8_t, svuint8_t,
	     z0 = svuqadd_s8_x (p0, z1, z4),
	     z0 = svuqadd_x (p0, z1, z4))

/*
** uqadd_w0_s8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s8_x_tied1, svint8_t, uint8_t,
		 z0 = svuqadd_n_s8_x (p0, z0, x0),
		 z0 = svuqadd_x (p0, z0, x0))

/*
** uqadd_w0_s8_x_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	suqadd	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s8_x_untied, svint8_t, uint8_t,
		 z0 = svuqadd_n_s8_x (p0, z1, x0),
		 z0 = svuqadd_x (p0, z1, x0))

/*
** uqadd_1_s8_x_tied1:
**	sqadd	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s8_x_tied1, svint8_t,
		z0 = svuqadd_n_s8_x (p0, z0, 1),
		z0 = svuqadd_x (p0, z0, 1))

/*
** uqadd_1_s8_x_untied:
**	movprfx	z0, z1
**	sqadd	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s8_x_untied, svint8_t,
		z0 = svuqadd_n_s8_x (p0, z1, 1),
		z0 = svuqadd_x (p0, z1, 1))

/*
** uqadd_127_s8_x:
**	sqadd	z0\.b, z0\.b, #127
**	ret
*/
TEST_UNIFORM_Z (uqadd_127_s8_x, svint8_t,
		z0 = svuqadd_n_s8_x (p0, z0, 127),
		z0 = svuqadd_x (p0, z0, 127))

/*
** uqadd_128_s8_x:
**	sqadd	z0\.b, z0\.b, #128
**	ret
*/
TEST_UNIFORM_Z (uqadd_128_s8_x, svint8_t,
		z0 = svuqadd_n_s8_x (p0, z0, 128),
		z0 = svuqadd_x (p0, z0, 128))

/*
** uqadd_255_s8_x:
**	sqadd	z0\.b, z0\.b, #255
**	ret
*/
TEST_UNIFORM_Z (uqadd_255_s8_x, svint8_t,
		z0 = svuqadd_n_s8_x (p0, z0, 255),
		z0 = svuqadd_x (p0, z0, 255))

/*
** uqadd_m1_s8_x:
**	sqadd	z0\.b, z0\.b, #255
**	ret
*/
TEST_UNIFORM_Z (uqadd_m1_s8_x, svint8_t,
		z0 = svuqadd_n_s8_x (p0, z0, -1),
		z0 = svuqadd_x (p0, z0, -1))

/*
** uqadd_m127_s8_x:
**	sqadd	z0\.b, z0\.b, #129
**	ret
*/
TEST_UNIFORM_Z (uqadd_m127_s8_x, svint8_t,
		z0 = svuqadd_n_s8_x (p0, z0, -127),
		z0 = svuqadd_x (p0, z0, -127))

/*
** uqadd_m128_s8_x:
**	sqadd	z0\.b, z0\.b, #128
**	ret
*/
TEST_UNIFORM_Z (uqadd_m128_s8_x, svint8_t,
		z0 = svuqadd_n_s8_x (p0, z0, -128),
		z0 = svuqadd_x (p0, z0, -128))
