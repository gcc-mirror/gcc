/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uqadd_s32_m_tied1:
**	suqadd	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (uqadd_s32_m_tied1, svint32_t, svuint32_t,
	     z0 = svuqadd_s32_m (p0, z0, z4),
	     z0 = svuqadd_m (p0, z0, z4))

/*
** uqadd_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	suqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (uqadd_s32_m_tied2, svint32_t, svuint32_t,
		 z0_res = svuqadd_s32_m (p0, z4, z0),
		 z0_res = svuqadd_m (p0, z4, z0))

/*
** uqadd_s32_m_untied:
**	movprfx	z0, z1
**	suqadd	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (uqadd_s32_m_untied, svint32_t, svuint32_t,
	     z0 = svuqadd_s32_m (p0, z1, z4),
	     z0 = svuqadd_m (p0, z1, z4))

/*
** uqadd_w0_s32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s32_m_tied1, svint32_t, uint32_t,
		 z0 = svuqadd_n_s32_m (p0, z0, x0),
		 z0 = svuqadd_m (p0, z0, x0))

/*
** uqadd_w0_s32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s32_m_untied, svint32_t, uint32_t,
		 z0 = svuqadd_n_s32_m (p0, z1, x0),
		 z0 = svuqadd_m (p0, z1, x0))

/*
** uqadd_1_s32_m_tied1:
**	mov	(z[0-9]+\.s), #1
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s32_m_tied1, svint32_t,
		z0 = svuqadd_n_s32_m (p0, z0, 1),
		z0 = svuqadd_m (p0, z0, 1))

/*
** uqadd_1_s32_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0, z1
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s32_m_untied, svint32_t,
		z0 = svuqadd_n_s32_m (p0, z1, 1),
		z0 = svuqadd_m (p0, z1, 1))

/*
** uqadd_127_s32_m:
**	mov	(z[0-9]+\.s), #127
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_127_s32_m, svint32_t,
		z0 = svuqadd_n_s32_m (p0, z0, 127),
		z0 = svuqadd_m (p0, z0, 127))

/*
** uqadd_128_s32_m:
**	mov	(z[0-9]+\.s), #128
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_128_s32_m, svint32_t,
		z0 = svuqadd_n_s32_m (p0, z0, 128),
		z0 = svuqadd_m (p0, z0, 128))

/*
** uqadd_255_s32_m:
**	mov	(z[0-9]+\.s), #255
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_255_s32_m, svint32_t,
		z0 = svuqadd_n_s32_m (p0, z0, 255),
		z0 = svuqadd_m (p0, z0, 255))

/*
** uqadd_m1_s32_m:
**	mov	(z[0-9]+)\.b, #-1
**	suqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (uqadd_m1_s32_m, svint32_t,
		z0 = svuqadd_n_s32_m (p0, z0, -1),
		z0 = svuqadd_m (p0, z0, -1))

/*
** uqadd_m127_s32_m:
**	mov	(z[0-9]+\.s), #-127
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m127_s32_m, svint32_t,
		z0 = svuqadd_n_s32_m (p0, z0, -127),
		z0 = svuqadd_m (p0, z0, -127))

/*
** uqadd_m128_s32_m:
**	mov	(z[0-9]+\.s), #-128
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m128_s32_m, svint32_t,
		z0 = svuqadd_n_s32_m (p0, z0, -128),
		z0 = svuqadd_m (p0, z0, -128))

/*
** uqadd_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	suqadd	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (uqadd_s32_z_tied1, svint32_t, svuint32_t,
	     z0 = svuqadd_s32_z (p0, z0, z4),
	     z0 = svuqadd_z (p0, z0, z4))

/*
** uqadd_s32_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, z4\.s
**	suqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (uqadd_s32_z_tied2, svint32_t, svuint32_t,
		 z0_res = svuqadd_s32_z (p0, z4, z0),
		 z0_res = svuqadd_z (p0, z4, z0))

/*
** uqadd_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	suqadd	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (uqadd_s32_z_untied, svint32_t, svuint32_t,
	     z0 = svuqadd_s32_z (p0, z1, z4),
	     z0 = svuqadd_z (p0, z1, z4))

/*
** uqadd_w0_s32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s32_z_tied1, svint32_t, uint32_t,
		 z0 = svuqadd_n_s32_z (p0, z0, x0),
		 z0 = svuqadd_z (p0, z0, x0))

/*
** uqadd_w0_s32_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z1\.s
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s32_z_untied, svint32_t, uint32_t,
		 z0 = svuqadd_n_s32_z (p0, z1, x0),
		 z0 = svuqadd_z (p0, z1, x0))

/*
** uqadd_1_s32_z_tied1:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z0\.s
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s32_z_tied1, svint32_t,
		z0 = svuqadd_n_s32_z (p0, z0, 1),
		z0 = svuqadd_z (p0, z0, 1))

/*
** uqadd_1_s32_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z1\.s
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s32_z_untied, svint32_t,
		z0 = svuqadd_n_s32_z (p0, z1, 1),
		z0 = svuqadd_z (p0, z1, 1))

/*
** uqadd_127_s32_z:
**	mov	(z[0-9]+\.s), #127
**	movprfx	z0\.s, p0/z, z0\.s
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_127_s32_z, svint32_t,
		z0 = svuqadd_n_s32_z (p0, z0, 127),
		z0 = svuqadd_z (p0, z0, 127))

/*
** uqadd_128_s32_z:
**	mov	(z[0-9]+\.s), #128
**	movprfx	z0\.s, p0/z, z0\.s
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_128_s32_z, svint32_t,
		z0 = svuqadd_n_s32_z (p0, z0, 128),
		z0 = svuqadd_z (p0, z0, 128))

/*
** uqadd_255_s32_z:
**	mov	(z[0-9]+\.s), #255
**	movprfx	z0\.s, p0/z, z0\.s
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_255_s32_z, svint32_t,
		z0 = svuqadd_n_s32_z (p0, z0, 255),
		z0 = svuqadd_z (p0, z0, 255))

/*
** uqadd_m1_s32_z:
**	mov	(z[0-9]+)\.b, #-1
**	movprfx	z0\.s, p0/z, z0\.s
**	suqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (uqadd_m1_s32_z, svint32_t,
		z0 = svuqadd_n_s32_z (p0, z0, -1),
		z0 = svuqadd_z (p0, z0, -1))

/*
** uqadd_m127_s32_z:
**	mov	(z[0-9]+\.s), #-127
**	movprfx	z0\.s, p0/z, z0\.s
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m127_s32_z, svint32_t,
		z0 = svuqadd_n_s32_z (p0, z0, -127),
		z0 = svuqadd_z (p0, z0, -127))

/*
** uqadd_m128_s32_z:
**	mov	(z[0-9]+\.s), #-128
**	movprfx	z0\.s, p0/z, z0\.s
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m128_s32_z, svint32_t,
		z0 = svuqadd_n_s32_z (p0, z0, -128),
		z0 = svuqadd_z (p0, z0, -128))

/*
** uqadd_s32_x_tied1:
**	suqadd	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (uqadd_s32_x_tied1, svint32_t, svuint32_t,
	     z0 = svuqadd_s32_x (p0, z0, z4),
	     z0 = svuqadd_x (p0, z0, z4))

/*
** uqadd_s32_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	suqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (uqadd_s32_x_tied2, svint32_t, svuint32_t,
		 z0_res = svuqadd_s32_x (p0, z4, z0),
		 z0_res = svuqadd_x (p0, z4, z0))

/*
** uqadd_s32_x_untied:
**	movprfx	z0, z1
**	suqadd	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (uqadd_s32_x_untied, svint32_t, svuint32_t,
	     z0 = svuqadd_s32_x (p0, z1, z4),
	     z0 = svuqadd_x (p0, z1, z4))

/*
** uqadd_w0_s32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s32_x_tied1, svint32_t, uint32_t,
		 z0 = svuqadd_n_s32_x (p0, z0, x0),
		 z0 = svuqadd_x (p0, z0, x0))

/*
** uqadd_w0_s32_x_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_w0_s32_x_untied, svint32_t, uint32_t,
		 z0 = svuqadd_n_s32_x (p0, z1, x0),
		 z0 = svuqadd_x (p0, z1, x0))

/*
** uqadd_1_s32_x_tied1:
**	sqadd	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s32_x_tied1, svint32_t,
		z0 = svuqadd_n_s32_x (p0, z0, 1),
		z0 = svuqadd_x (p0, z0, 1))

/*
** uqadd_1_s32_x_untied:
**	movprfx	z0, z1
**	sqadd	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s32_x_untied, svint32_t,
		z0 = svuqadd_n_s32_x (p0, z1, 1),
		z0 = svuqadd_x (p0, z1, 1))

/*
** uqadd_127_s32_x:
**	sqadd	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (uqadd_127_s32_x, svint32_t,
		z0 = svuqadd_n_s32_x (p0, z0, 127),
		z0 = svuqadd_x (p0, z0, 127))

/*
** uqadd_128_s32_x:
**	sqadd	z0\.s, z0\.s, #128
**	ret
*/
TEST_UNIFORM_Z (uqadd_128_s32_x, svint32_t,
		z0 = svuqadd_n_s32_x (p0, z0, 128),
		z0 = svuqadd_x (p0, z0, 128))

/*
** uqadd_255_s32_x:
**	sqadd	z0\.s, z0\.s, #255
**	ret
*/
TEST_UNIFORM_Z (uqadd_255_s32_x, svint32_t,
		z0 = svuqadd_n_s32_x (p0, z0, 255),
		z0 = svuqadd_x (p0, z0, 255))

/*
** uqadd_m1_s32_x:
**	mov	(z[0-9]+)\.b, #-1
**	suqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (uqadd_m1_s32_x, svint32_t,
		z0 = svuqadd_n_s32_x (p0, z0, -1),
		z0 = svuqadd_x (p0, z0, -1))

/*
** uqadd_m127_s32_x:
**	mov	(z[0-9]+\.s), #-127
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m127_s32_x, svint32_t,
		z0 = svuqadd_n_s32_x (p0, z0, -127),
		z0 = svuqadd_x (p0, z0, -127))

/*
** uqadd_m128_s32_x:
**	mov	(z[0-9]+\.s), #-128
**	suqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m128_s32_x, svint32_t,
		z0 = svuqadd_n_s32_x (p0, z0, -128),
		z0 = svuqadd_x (p0, z0, -128))
