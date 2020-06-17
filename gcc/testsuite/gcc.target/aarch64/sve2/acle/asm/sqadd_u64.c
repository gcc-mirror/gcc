/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sqadd_u64_m_tied1:
**	usqadd	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (sqadd_u64_m_tied1, svuint64_t, svint64_t,
	     z0 = svsqadd_u64_m (p0, z0, z4),
	     z0 = svsqadd_m (p0, z0, z4))

/*
** sqadd_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (sqadd_u64_m_tied2, svuint64_t, svint64_t,
		 z0_res = svsqadd_u64_m (p0, z4, z0),
		 z0_res = svsqadd_m (p0, z4, z0))

/*
** sqadd_u64_m_untied:
**	movprfx	z0, z1
**	usqadd	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (sqadd_u64_m_untied, svuint64_t, svint64_t,
	     z0 = svsqadd_u64_m (p0, z1, z4),
	     z0 = svsqadd_m (p0, z1, z4))

/*
** sqadd_x0_u64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_x0_u64_m_tied1, svuint64_t, int64_t,
		 z0 = svsqadd_n_u64_m (p0, z0, x0),
		 z0 = svsqadd_m (p0, z0, x0))

/*
** sqadd_x0_u64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_x0_u64_m_untied, svuint64_t, int64_t,
		 z0 = svsqadd_n_u64_m (p0, z1, x0),
		 z0 = svsqadd_m (p0, z1, x0))

/*
** sqadd_1_u64_m_tied1:
**	mov	(z[0-9]+\.d), #1
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u64_m_tied1, svuint64_t,
		z0 = svsqadd_n_u64_m (p0, z0, 1),
		z0 = svsqadd_m (p0, z0, 1))

/*
** sqadd_1_u64_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0, z1
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u64_m_untied, svuint64_t,
		z0 = svsqadd_n_u64_m (p0, z1, 1),
		z0 = svsqadd_m (p0, z1, 1))

/*
** sqadd_127_u64_m:
**	mov	(z[0-9]+\.d), #127
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_127_u64_m, svuint64_t,
		z0 = svsqadd_n_u64_m (p0, z0, 127),
		z0 = svsqadd_m (p0, z0, 127))

/*
** sqadd_128_u64_m:
**	mov	(z[0-9]+\.d), #128
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_128_u64_m, svuint64_t,
		z0 = svsqadd_n_u64_m (p0, z0, 128),
		z0 = svsqadd_m (p0, z0, 128))

/*
** sqadd_255_u64_m:
**	mov	(z[0-9]+\.d), #255
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_255_u64_m, svuint64_t,
		z0 = svsqadd_n_u64_m (p0, z0, 255),
		z0 = svsqadd_m (p0, z0, 255))

/*
** sqadd_m1_u64_m:
**	mov	(z[0-9]+)\.b, #-1
**	usqadd	z0\.d, p0/m, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (sqadd_m1_u64_m, svuint64_t,
		z0 = svsqadd_n_u64_m (p0, z0, -1),
		z0 = svsqadd_m (p0, z0, -1))

/*
** sqadd_m127_u64_m:
**	mov	(z[0-9]+\.d), #-127
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m127_u64_m, svuint64_t,
		z0 = svsqadd_n_u64_m (p0, z0, -127),
		z0 = svsqadd_m (p0, z0, -127))

/*
** sqadd_m128_u64_m:
**	mov	(z[0-9]+\.d), #-128
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m128_u64_m, svuint64_t,
		z0 = svsqadd_n_u64_m (p0, z0, -128),
		z0 = svsqadd_m (p0, z0, -128))

/*
** sqadd_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	usqadd	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (sqadd_u64_z_tied1, svuint64_t, svint64_t,
	     z0 = svsqadd_u64_z (p0, z0, z4),
	     z0 = svsqadd_z (p0, z0, z4))

/*
** sqadd_u64_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, z4\.d
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (sqadd_u64_z_tied2, svuint64_t, svint64_t,
		 z0_res = svsqadd_u64_z (p0, z4, z0),
		 z0_res = svsqadd_z (p0, z4, z0))

/*
** sqadd_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	usqadd	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (sqadd_u64_z_untied, svuint64_t, svint64_t,
	     z0 = svsqadd_u64_z (p0, z1, z4),
	     z0 = svsqadd_z (p0, z1, z4))

/*
** sqadd_x0_u64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_x0_u64_z_tied1, svuint64_t, int64_t,
		 z0 = svsqadd_n_u64_z (p0, z0, x0),
		 z0 = svsqadd_z (p0, z0, x0))

/*
** sqadd_x0_u64_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z1\.d
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_x0_u64_z_untied, svuint64_t, int64_t,
		 z0 = svsqadd_n_u64_z (p0, z1, x0),
		 z0 = svsqadd_z (p0, z1, x0))

/*
** sqadd_1_u64_z_tied1:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0\.d, p0/z, z0\.d
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u64_z_tied1, svuint64_t,
		z0 = svsqadd_n_u64_z (p0, z0, 1),
		z0 = svsqadd_z (p0, z0, 1))

/*
** sqadd_1_u64_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0\.d, p0/z, z1\.d
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u64_z_untied, svuint64_t,
		z0 = svsqadd_n_u64_z (p0, z1, 1),
		z0 = svsqadd_z (p0, z1, 1))

/*
** sqadd_127_u64_z:
**	mov	(z[0-9]+\.d), #127
**	movprfx	z0\.d, p0/z, z0\.d
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_127_u64_z, svuint64_t,
		z0 = svsqadd_n_u64_z (p0, z0, 127),
		z0 = svsqadd_z (p0, z0, 127))

/*
** sqadd_128_u64_z:
**	mov	(z[0-9]+\.d), #128
**	movprfx	z0\.d, p0/z, z0\.d
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_128_u64_z, svuint64_t,
		z0 = svsqadd_n_u64_z (p0, z0, 128),
		z0 = svsqadd_z (p0, z0, 128))

/*
** sqadd_255_u64_z:
**	mov	(z[0-9]+\.d), #255
**	movprfx	z0\.d, p0/z, z0\.d
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_255_u64_z, svuint64_t,
		z0 = svsqadd_n_u64_z (p0, z0, 255),
		z0 = svsqadd_z (p0, z0, 255))

/*
** sqadd_m1_u64_z:
**	mov	(z[0-9]+)\.b, #-1
**	movprfx	z0\.d, p0/z, z0\.d
**	usqadd	z0\.d, p0/m, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (sqadd_m1_u64_z, svuint64_t,
		z0 = svsqadd_n_u64_z (p0, z0, -1),
		z0 = svsqadd_z (p0, z0, -1))

/*
** sqadd_m127_u64_z:
**	mov	(z[0-9]+\.d), #-127
**	movprfx	z0\.d, p0/z, z0\.d
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m127_u64_z, svuint64_t,
		z0 = svsqadd_n_u64_z (p0, z0, -127),
		z0 = svsqadd_z (p0, z0, -127))

/*
** sqadd_m128_u64_z:
**	mov	(z[0-9]+\.d), #-128
**	movprfx	z0\.d, p0/z, z0\.d
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m128_u64_z, svuint64_t,
		z0 = svsqadd_n_u64_z (p0, z0, -128),
		z0 = svsqadd_z (p0, z0, -128))

/*
** sqadd_u64_x_tied1:
**	usqadd	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (sqadd_u64_x_tied1, svuint64_t, svint64_t,
	     z0 = svsqadd_u64_x (p0, z0, z4),
	     z0 = svsqadd_x (p0, z0, z4))

/*
** sqadd_u64_x_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (sqadd_u64_x_tied2, svuint64_t, svint64_t,
		 z0_res = svsqadd_u64_x (p0, z4, z0),
		 z0_res = svsqadd_x (p0, z4, z0))

/*
** sqadd_u64_x_untied:
**	movprfx	z0, z1
**	usqadd	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (sqadd_u64_x_untied, svuint64_t, svint64_t,
	     z0 = svsqadd_u64_x (p0, z1, z4),
	     z0 = svsqadd_x (p0, z1, z4))

/*
** sqadd_x0_u64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_x0_u64_x_tied1, svuint64_t, int64_t,
		 z0 = svsqadd_n_u64_x (p0, z0, x0),
		 z0 = svsqadd_x (p0, z0, x0))

/*
** sqadd_x0_u64_x_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (sqadd_x0_u64_x_untied, svuint64_t, int64_t,
		 z0 = svsqadd_n_u64_x (p0, z1, x0),
		 z0 = svsqadd_x (p0, z1, x0))

/*
** sqadd_1_u64_x_tied1:
**	uqadd	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u64_x_tied1, svuint64_t,
		z0 = svsqadd_n_u64_x (p0, z0, 1),
		z0 = svsqadd_x (p0, z0, 1))

/*
** sqadd_1_u64_x_untied:
**	movprfx	z0, z1
**	uqadd	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (sqadd_1_u64_x_untied, svuint64_t,
		z0 = svsqadd_n_u64_x (p0, z1, 1),
		z0 = svsqadd_x (p0, z1, 1))

/*
** sqadd_127_u64_x:
**	uqadd	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (sqadd_127_u64_x, svuint64_t,
		z0 = svsqadd_n_u64_x (p0, z0, 127),
		z0 = svsqadd_x (p0, z0, 127))

/*
** sqadd_128_u64_x:
**	uqadd	z0\.d, z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (sqadd_128_u64_x, svuint64_t,
		z0 = svsqadd_n_u64_x (p0, z0, 128),
		z0 = svsqadd_x (p0, z0, 128))

/*
** sqadd_255_u64_x:
**	uqadd	z0\.d, z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (sqadd_255_u64_x, svuint64_t,
		z0 = svsqadd_n_u64_x (p0, z0, 255),
		z0 = svsqadd_x (p0, z0, 255))

/*
** sqadd_m1_u64_x:
**	mov	(z[0-9]+)\.b, #-1
**	usqadd	z0\.d, p0/m, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (sqadd_m1_u64_x, svuint64_t,
		z0 = svsqadd_n_u64_x (p0, z0, -1),
		z0 = svsqadd_x (p0, z0, -1))

/*
** sqadd_m127_u64_x:
**	mov	(z[0-9]+\.d), #-127
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m127_u64_x, svuint64_t,
		z0 = svsqadd_n_u64_x (p0, z0, -127),
		z0 = svsqadd_x (p0, z0, -127))

/*
** sqadd_m128_u64_x:
**	mov	(z[0-9]+\.d), #-128
**	usqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (sqadd_m128_u64_x, svuint64_t,
		z0 = svsqadd_n_u64_x (p0, z0, -128),
		z0 = svsqadd_x (p0, z0, -128))
