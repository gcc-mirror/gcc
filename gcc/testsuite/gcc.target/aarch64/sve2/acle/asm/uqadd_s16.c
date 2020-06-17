/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uqadd_s16_m_tied1:
**	suqadd	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (uqadd_s16_m_tied1, svint16_t, svuint16_t,
	     z0 = svuqadd_s16_m (p0, z0, z4),
	     z0 = svuqadd_m (p0, z0, z4))

/*
** uqadd_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	suqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (uqadd_s16_m_tied2, svint16_t, svuint16_t,
		 z0_res = svuqadd_s16_m (p0, z4, z0),
		 z0_res = svuqadd_m (p0, z4, z0))

/*
** uqadd_s16_m_untied:
**	movprfx	z0, z1
**	suqadd	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (uqadd_s16_m_untied, svint16_t, svuint16_t,
	     z0 = svuqadd_s16_m (p0, z1, z4),
	     z0 = svuqadd_m (p0, z1, z4))

/*
** uqadd_w0_s16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s16_m_tied1, svint16_t, uint16_t,
		 z0 = svuqadd_n_s16_m (p0, z0, x0),
		 z0 = svuqadd_m (p0, z0, x0))

/*
** uqadd_w0_s16_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s16_m_untied, svint16_t, uint16_t,
		 z0 = svuqadd_n_s16_m (p0, z1, x0),
		 z0 = svuqadd_m (p0, z1, x0))

/*
** uqadd_1_s16_m_tied1:
**	mov	(z[0-9]+\.h), #1
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s16_m_tied1, svint16_t,
		z0 = svuqadd_n_s16_m (p0, z0, 1),
		z0 = svuqadd_m (p0, z0, 1))

/*
** uqadd_1_s16_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0, z1
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s16_m_untied, svint16_t,
		z0 = svuqadd_n_s16_m (p0, z1, 1),
		z0 = svuqadd_m (p0, z1, 1))

/*
** uqadd_127_s16_m:
**	mov	(z[0-9]+\.h), #127
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_127_s16_m, svint16_t,
		z0 = svuqadd_n_s16_m (p0, z0, 127),
		z0 = svuqadd_m (p0, z0, 127))

/*
** uqadd_128_s16_m:
**	mov	(z[0-9]+\.h), #128
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_128_s16_m, svint16_t,
		z0 = svuqadd_n_s16_m (p0, z0, 128),
		z0 = svuqadd_m (p0, z0, 128))

/*
** uqadd_255_s16_m:
**	mov	(z[0-9]+\.h), #255
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_255_s16_m, svint16_t,
		z0 = svuqadd_n_s16_m (p0, z0, 255),
		z0 = svuqadd_m (p0, z0, 255))

/*
** uqadd_m1_s16_m:
**	mov	(z[0-9]+)\.b, #-1
**	suqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (uqadd_m1_s16_m, svint16_t,
		z0 = svuqadd_n_s16_m (p0, z0, -1),
		z0 = svuqadd_m (p0, z0, -1))

/*
** uqadd_m127_s16_m:
**	mov	(z[0-9]+\.h), #-127
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m127_s16_m, svint16_t,
		z0 = svuqadd_n_s16_m (p0, z0, -127),
		z0 = svuqadd_m (p0, z0, -127))

/*
** uqadd_m128_s16_m:
**	mov	(z[0-9]+\.h), #-128
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m128_s16_m, svint16_t,
		z0 = svuqadd_n_s16_m (p0, z0, -128),
		z0 = svuqadd_m (p0, z0, -128))

/*
** uqadd_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	suqadd	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (uqadd_s16_z_tied1, svint16_t, svuint16_t,
	     z0 = svuqadd_s16_z (p0, z0, z4),
	     z0 = svuqadd_z (p0, z0, z4))

/*
** uqadd_s16_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z4\.h
**	suqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (uqadd_s16_z_tied2, svint16_t, svuint16_t,
		 z0_res = svuqadd_s16_z (p0, z4, z0),
		 z0_res = svuqadd_z (p0, z4, z0))

/*
** uqadd_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	suqadd	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (uqadd_s16_z_untied, svint16_t, svuint16_t,
	     z0 = svuqadd_s16_z (p0, z1, z4),
	     z0 = svuqadd_z (p0, z1, z4))

/*
** uqadd_w0_s16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s16_z_tied1, svint16_t, uint16_t,
		 z0 = svuqadd_n_s16_z (p0, z0, x0),
		 z0 = svuqadd_z (p0, z0, x0))

/*
** uqadd_w0_s16_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z1\.h
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s16_z_untied, svint16_t, uint16_t,
		 z0 = svuqadd_n_s16_z (p0, z1, x0),
		 z0 = svuqadd_z (p0, z1, x0))

/*
** uqadd_1_s16_z_tied1:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z0\.h
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s16_z_tied1, svint16_t,
		z0 = svuqadd_n_s16_z (p0, z0, 1),
		z0 = svuqadd_z (p0, z0, 1))

/*
** uqadd_1_s16_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z1\.h
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s16_z_untied, svint16_t,
		z0 = svuqadd_n_s16_z (p0, z1, 1),
		z0 = svuqadd_z (p0, z1, 1))

/*
** uqadd_127_s16_z:
**	mov	(z[0-9]+\.h), #127
**	movprfx	z0\.h, p0/z, z0\.h
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_127_s16_z, svint16_t,
		z0 = svuqadd_n_s16_z (p0, z0, 127),
		z0 = svuqadd_z (p0, z0, 127))

/*
** uqadd_128_s16_z:
**	mov	(z[0-9]+\.h), #128
**	movprfx	z0\.h, p0/z, z0\.h
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_128_s16_z, svint16_t,
		z0 = svuqadd_n_s16_z (p0, z0, 128),
		z0 = svuqadd_z (p0, z0, 128))

/*
** uqadd_255_s16_z:
**	mov	(z[0-9]+\.h), #255
**	movprfx	z0\.h, p0/z, z0\.h
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_255_s16_z, svint16_t,
		z0 = svuqadd_n_s16_z (p0, z0, 255),
		z0 = svuqadd_z (p0, z0, 255))

/*
** uqadd_m1_s16_z:
**	mov	(z[0-9]+)\.b, #-1
**	movprfx	z0\.h, p0/z, z0\.h
**	suqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (uqadd_m1_s16_z, svint16_t,
		z0 = svuqadd_n_s16_z (p0, z0, -1),
		z0 = svuqadd_z (p0, z0, -1))

/*
** uqadd_m127_s16_z:
**	mov	(z[0-9]+\.h), #-127
**	movprfx	z0\.h, p0/z, z0\.h
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m127_s16_z, svint16_t,
		z0 = svuqadd_n_s16_z (p0, z0, -127),
		z0 = svuqadd_z (p0, z0, -127))

/*
** uqadd_m128_s16_z:
**	mov	(z[0-9]+\.h), #-128
**	movprfx	z0\.h, p0/z, z0\.h
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m128_s16_z, svint16_t,
		z0 = svuqadd_n_s16_z (p0, z0, -128),
		z0 = svuqadd_z (p0, z0, -128))

/*
** uqadd_s16_x_tied1:
**	suqadd	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (uqadd_s16_x_tied1, svint16_t, svuint16_t,
	     z0 = svuqadd_s16_x (p0, z0, z4),
	     z0 = svuqadd_x (p0, z0, z4))

/*
** uqadd_s16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	suqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (uqadd_s16_x_tied2, svint16_t, svuint16_t,
		 z0_res = svuqadd_s16_x (p0, z4, z0),
		 z0_res = svuqadd_x (p0, z4, z0))

/*
** uqadd_s16_x_untied:
**	movprfx	z0, z1
**	suqadd	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (uqadd_s16_x_untied, svint16_t, svuint16_t,
	     z0 = svuqadd_s16_x (p0, z1, z4),
	     z0 = svuqadd_x (p0, z1, z4))

/*
** uqadd_w0_s16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s16_x_tied1, svint16_t, uint16_t,
		 z0 = svuqadd_n_s16_x (p0, z0, x0),
		 z0 = svuqadd_x (p0, z0, x0))

/*
** uqadd_w0_s16_x_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s16_x_untied, svint16_t, uint16_t,
		 z0 = svuqadd_n_s16_x (p0, z1, x0),
		 z0 = svuqadd_x (p0, z1, x0))

/*
** uqadd_1_s16_x_tied1:
**	sqadd	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s16_x_tied1, svint16_t,
		z0 = svuqadd_n_s16_x (p0, z0, 1),
		z0 = svuqadd_x (p0, z0, 1))

/*
** uqadd_1_s16_x_untied:
**	movprfx	z0, z1
**	sqadd	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s16_x_untied, svint16_t,
		z0 = svuqadd_n_s16_x (p0, z1, 1),
		z0 = svuqadd_x (p0, z1, 1))

/*
** uqadd_127_s16_x:
**	sqadd	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (uqadd_127_s16_x, svint16_t,
		z0 = svuqadd_n_s16_x (p0, z0, 127),
		z0 = svuqadd_x (p0, z0, 127))

/*
** uqadd_128_s16_x:
**	sqadd	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (uqadd_128_s16_x, svint16_t,
		z0 = svuqadd_n_s16_x (p0, z0, 128),
		z0 = svuqadd_x (p0, z0, 128))

/*
** uqadd_255_s16_x:
**	sqadd	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (uqadd_255_s16_x, svint16_t,
		z0 = svuqadd_n_s16_x (p0, z0, 255),
		z0 = svuqadd_x (p0, z0, 255))

/*
** uqadd_m1_s16_x:
**	mov	(z[0-9]+)\.b, #-1
**	suqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (uqadd_m1_s16_x, svint16_t,
		z0 = svuqadd_n_s16_x (p0, z0, -1),
		z0 = svuqadd_x (p0, z0, -1))

/*
** uqadd_m127_s16_x:
**	mov	(z[0-9]+\.h), #-127
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m127_s16_x, svint16_t,
		z0 = svuqadd_n_s16_x (p0, z0, -127),
		z0 = svuqadd_x (p0, z0, -127))

/*
** uqadd_m128_s16_x:
**	mov	(z[0-9]+\.h), #-128
**	suqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m128_s16_x, svint16_t,
		z0 = svuqadd_n_s16_x (p0, z0, -128),
		z0 = svuqadd_x (p0, z0, -128))
