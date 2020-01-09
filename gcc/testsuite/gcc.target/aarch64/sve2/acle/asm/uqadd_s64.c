/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uqadd_s64_m_tied1:
**	suqadd	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (uqadd_s64_m_tied1, svint64_t, svuint64_t,
	     z0 = svuqadd_s64_m (p0, z0, z4),
	     z0 = svuqadd_m (p0, z0, z4))

/*
** uqadd_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (uqadd_s64_m_tied2, svint64_t, svuint64_t,
		 z0_res = svuqadd_s64_m (p0, z4, z0),
		 z0_res = svuqadd_m (p0, z4, z0))

/*
** uqadd_s64_m_untied:
**	movprfx	z0, z1
**	suqadd	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (uqadd_s64_m_untied, svint64_t, svuint64_t,
	     z0 = svuqadd_s64_m (p0, z1, z4),
	     z0 = svuqadd_m (p0, z1, z4))

/*
** uqadd_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_x0_s64_m_tied1, svint64_t, uint64_t,
		 z0 = svuqadd_n_s64_m (p0, z0, x0),
		 z0 = svuqadd_m (p0, z0, x0))

/*
** uqadd_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_x0_s64_m_untied, svint64_t, uint64_t,
		 z0 = svuqadd_n_s64_m (p0, z1, x0),
		 z0 = svuqadd_m (p0, z1, x0))

/*
** uqadd_1_s64_m_tied1:
**	mov	(z[0-9]+\.d), #1
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s64_m_tied1, svint64_t,
		z0 = svuqadd_n_s64_m (p0, z0, 1),
		z0 = svuqadd_m (p0, z0, 1))

/*
** uqadd_1_s64_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0, z1
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s64_m_untied, svint64_t,
		z0 = svuqadd_n_s64_m (p0, z1, 1),
		z0 = svuqadd_m (p0, z1, 1))

/*
** uqadd_127_s64_m:
**	mov	(z[0-9]+\.d), #127
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_127_s64_m, svint64_t,
		z0 = svuqadd_n_s64_m (p0, z0, 127),
		z0 = svuqadd_m (p0, z0, 127))

/*
** uqadd_128_s64_m:
**	mov	(z[0-9]+\.d), #128
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_128_s64_m, svint64_t,
		z0 = svuqadd_n_s64_m (p0, z0, 128),
		z0 = svuqadd_m (p0, z0, 128))

/*
** uqadd_255_s64_m:
**	mov	(z[0-9]+\.d), #255
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_255_s64_m, svint64_t,
		z0 = svuqadd_n_s64_m (p0, z0, 255),
		z0 = svuqadd_m (p0, z0, 255))

/*
** uqadd_m1_s64_m:
**	mov	(z[0-9]+)\.b, #-1
**	suqadd	z0\.d, p0/m, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (uqadd_m1_s64_m, svint64_t,
		z0 = svuqadd_n_s64_m (p0, z0, -1),
		z0 = svuqadd_m (p0, z0, -1))

/*
** uqadd_m127_s64_m:
**	mov	(z[0-9]+\.d), #-127
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m127_s64_m, svint64_t,
		z0 = svuqadd_n_s64_m (p0, z0, -127),
		z0 = svuqadd_m (p0, z0, -127))

/*
** uqadd_m128_s64_m:
**	mov	(z[0-9]+\.d), #-128
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m128_s64_m, svint64_t,
		z0 = svuqadd_n_s64_m (p0, z0, -128),
		z0 = svuqadd_m (p0, z0, -128))

/*
** uqadd_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	suqadd	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (uqadd_s64_z_tied1, svint64_t, svuint64_t,
	     z0 = svuqadd_s64_z (p0, z0, z4),
	     z0 = svuqadd_z (p0, z0, z4))

/*
** uqadd_s64_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, z4\.d
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (uqadd_s64_z_tied2, svint64_t, svuint64_t,
		 z0_res = svuqadd_s64_z (p0, z4, z0),
		 z0_res = svuqadd_z (p0, z4, z0))

/*
** uqadd_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	suqadd	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (uqadd_s64_z_untied, svint64_t, svuint64_t,
	     z0 = svuqadd_s64_z (p0, z1, z4),
	     z0 = svuqadd_z (p0, z1, z4))

/*
** uqadd_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_x0_s64_z_tied1, svint64_t, uint64_t,
		 z0 = svuqadd_n_s64_z (p0, z0, x0),
		 z0 = svuqadd_z (p0, z0, x0))

/*
** uqadd_x0_s64_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z1\.d
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_x0_s64_z_untied, svint64_t, uint64_t,
		 z0 = svuqadd_n_s64_z (p0, z1, x0),
		 z0 = svuqadd_z (p0, z1, x0))

/*
** uqadd_1_s64_z_tied1:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0\.d, p0/z, z0\.d
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s64_z_tied1, svint64_t,
		z0 = svuqadd_n_s64_z (p0, z0, 1),
		z0 = svuqadd_z (p0, z0, 1))

/*
** uqadd_1_s64_z_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0\.d, p0/z, z1\.d
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s64_z_untied, svint64_t,
		z0 = svuqadd_n_s64_z (p0, z1, 1),
		z0 = svuqadd_z (p0, z1, 1))

/*
** uqadd_127_s64_z:
**	mov	(z[0-9]+\.d), #127
**	movprfx	z0\.d, p0/z, z0\.d
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_127_s64_z, svint64_t,
		z0 = svuqadd_n_s64_z (p0, z0, 127),
		z0 = svuqadd_z (p0, z0, 127))

/*
** uqadd_128_s64_z:
**	mov	(z[0-9]+\.d), #128
**	movprfx	z0\.d, p0/z, z0\.d
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_128_s64_z, svint64_t,
		z0 = svuqadd_n_s64_z (p0, z0, 128),
		z0 = svuqadd_z (p0, z0, 128))

/*
** uqadd_255_s64_z:
**	mov	(z[0-9]+\.d), #255
**	movprfx	z0\.d, p0/z, z0\.d
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_255_s64_z, svint64_t,
		z0 = svuqadd_n_s64_z (p0, z0, 255),
		z0 = svuqadd_z (p0, z0, 255))

/*
** uqadd_m1_s64_z:
**	mov	(z[0-9]+)\.b, #-1
**	movprfx	z0\.d, p0/z, z0\.d
**	suqadd	z0\.d, p0/m, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (uqadd_m1_s64_z, svint64_t,
		z0 = svuqadd_n_s64_z (p0, z0, -1),
		z0 = svuqadd_z (p0, z0, -1))

/*
** uqadd_m127_s64_z:
**	mov	(z[0-9]+\.d), #-127
**	movprfx	z0\.d, p0/z, z0\.d
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m127_s64_z, svint64_t,
		z0 = svuqadd_n_s64_z (p0, z0, -127),
		z0 = svuqadd_z (p0, z0, -127))

/*
** uqadd_m128_s64_z:
**	mov	(z[0-9]+\.d), #-128
**	movprfx	z0\.d, p0/z, z0\.d
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m128_s64_z, svint64_t,
		z0 = svuqadd_n_s64_z (p0, z0, -128),
		z0 = svuqadd_z (p0, z0, -128))

/*
** uqadd_s64_x_tied1:
**	suqadd	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (uqadd_s64_x_tied1, svint64_t, svuint64_t,
	     z0 = svuqadd_s64_x (p0, z0, z4),
	     z0 = svuqadd_x (p0, z0, z4))

/*
** uqadd_s64_x_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (uqadd_s64_x_tied2, svint64_t, svuint64_t,
		 z0_res = svuqadd_s64_x (p0, z4, z0),
		 z0_res = svuqadd_x (p0, z4, z0))

/*
** uqadd_s64_x_untied:
**	movprfx	z0, z1
**	suqadd	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (uqadd_s64_x_untied, svint64_t, svuint64_t,
	     z0 = svuqadd_s64_x (p0, z1, z4),
	     z0 = svuqadd_x (p0, z1, z4))

/*
** uqadd_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_x0_s64_x_tied1, svint64_t, uint64_t,
		 z0 = svuqadd_n_s64_x (p0, z0, x0),
		 z0 = svuqadd_x (p0, z0, x0))

/*
** uqadd_x0_s64_x_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (uqadd_x0_s64_x_untied, svint64_t, uint64_t,
		 z0 = svuqadd_n_s64_x (p0, z1, x0),
		 z0 = svuqadd_x (p0, z1, x0))

/*
** uqadd_1_s64_x_tied1:
**	sqadd	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s64_x_tied1, svint64_t,
		z0 = svuqadd_n_s64_x (p0, z0, 1),
		z0 = svuqadd_x (p0, z0, 1))

/*
** uqadd_1_s64_x_untied:
**	movprfx	z0, z1
**	sqadd	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (uqadd_1_s64_x_untied, svint64_t,
		z0 = svuqadd_n_s64_x (p0, z1, 1),
		z0 = svuqadd_x (p0, z1, 1))

/*
** uqadd_127_s64_x:
**	sqadd	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (uqadd_127_s64_x, svint64_t,
		z0 = svuqadd_n_s64_x (p0, z0, 127),
		z0 = svuqadd_x (p0, z0, 127))

/*
** uqadd_128_s64_x:
**	sqadd	z0\.d, z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (uqadd_128_s64_x, svint64_t,
		z0 = svuqadd_n_s64_x (p0, z0, 128),
		z0 = svuqadd_x (p0, z0, 128))

/*
** uqadd_255_s64_x:
**	sqadd	z0\.d, z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (uqadd_255_s64_x, svint64_t,
		z0 = svuqadd_n_s64_x (p0, z0, 255),
		z0 = svuqadd_x (p0, z0, 255))

/*
** uqadd_m1_s64_x:
**	mov	(z[0-9]+)\.b, #-1
**	suqadd	z0\.d, p0/m, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (uqadd_m1_s64_x, svint64_t,
		z0 = svuqadd_n_s64_x (p0, z0, -1),
		z0 = svuqadd_x (p0, z0, -1))

/*
** uqadd_m127_s64_x:
**	mov	(z[0-9]+\.d), #-127
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m127_s64_x, svint64_t,
		z0 = svuqadd_n_s64_x (p0, z0, -127),
		z0 = svuqadd_x (p0, z0, -127))

/*
** uqadd_m128_s64_x:
**	mov	(z[0-9]+\.d), #-128
**	suqadd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (uqadd_m128_s64_x, svint64_t,
		z0 = svuqadd_n_s64_x (p0, z0, -128),
		z0 = svuqadd_x (p0, z0, -128))
