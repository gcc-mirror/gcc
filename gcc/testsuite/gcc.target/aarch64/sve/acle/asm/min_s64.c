/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** min_s64_m_tied1:
**	smin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (min_s64_m_tied1, svint64_t,
		z0 = svmin_s64_m (p0, z0, z1),
		z0 = svmin_m (p0, z0, z1))

/*
** min_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	smin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (min_s64_m_tied2, svint64_t,
		z0 = svmin_s64_m (p0, z1, z0),
		z0 = svmin_m (p0, z1, z0))

/*
** min_s64_m_untied:
**	movprfx	z0, z1
**	smin	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (min_s64_m_untied, svint64_t,
		z0 = svmin_s64_m (p0, z1, z2),
		z0 = svmin_m (p0, z1, z2))

/*
** min_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	smin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (min_x0_s64_m_tied1, svint64_t, int64_t,
		 z0 = svmin_n_s64_m (p0, z0, x0),
		 z0 = svmin_m (p0, z0, x0))

/*
** min_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	smin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (min_x0_s64_m_untied, svint64_t, int64_t,
		 z0 = svmin_n_s64_m (p0, z1, x0),
		 z0 = svmin_m (p0, z1, x0))

/*
** min_1_s64_m_tied1:
**	mov	(z[0-9]+\.d), #1
**	smin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_s64_m_tied1, svint64_t,
		z0 = svmin_n_s64_m (p0, z0, 1),
		z0 = svmin_m (p0, z0, 1))

/*
** min_1_s64_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0, z1
**	smin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_s64_m_untied, svint64_t,
		z0 = svmin_n_s64_m (p0, z1, 1),
		z0 = svmin_m (p0, z1, 1))

/*
** min_m1_s64_m:
**	mov	(z[0-9]+)\.b, #-1
**	smin	z0\.d, p0/m, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (min_m1_s64_m, svint64_t,
		z0 = svmin_n_s64_m (p0, z0, -1),
		z0 = svmin_m (p0, z0, -1))

/*
** min_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	smin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (min_s64_z_tied1, svint64_t,
		z0 = svmin_s64_z (p0, z0, z1),
		z0 = svmin_z (p0, z0, z1))

/*
** min_s64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	smin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (min_s64_z_tied2, svint64_t,
		z0 = svmin_s64_z (p0, z1, z0),
		z0 = svmin_z (p0, z1, z0))

/*
** min_s64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	smin	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	smin	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (min_s64_z_untied, svint64_t,
		z0 = svmin_s64_z (p0, z1, z2),
		z0 = svmin_z (p0, z1, z2))

/*
** min_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	smin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (min_x0_s64_z_tied1, svint64_t, int64_t,
		 z0 = svmin_n_s64_z (p0, z0, x0),
		 z0 = svmin_z (p0, z0, x0))

/*
** min_x0_s64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	smin	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	smin	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (min_x0_s64_z_untied, svint64_t, int64_t,
		 z0 = svmin_n_s64_z (p0, z1, x0),
		 z0 = svmin_z (p0, z1, x0))

/*
** min_1_s64_z_tied1:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0\.d, p0/z, z0\.d
**	smin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_s64_z_tied1, svint64_t,
		z0 = svmin_n_s64_z (p0, z0, 1),
		z0 = svmin_z (p0, z0, 1))

/*
** min_1_s64_z_untied:
**	mov	(z[0-9]+\.d), #1
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	smin	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	smin	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (min_1_s64_z_untied, svint64_t,
		z0 = svmin_n_s64_z (p0, z1, 1),
		z0 = svmin_z (p0, z1, 1))

/*
** min_s64_x_tied1:
**	smin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (min_s64_x_tied1, svint64_t,
		z0 = svmin_s64_x (p0, z0, z1),
		z0 = svmin_x (p0, z0, z1))

/*
** min_s64_x_tied2:
**	smin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (min_s64_x_tied2, svint64_t,
		z0 = svmin_s64_x (p0, z1, z0),
		z0 = svmin_x (p0, z1, z0))

/*
** min_s64_x_untied:
** (
**	movprfx	z0, z1
**	smin	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0, z2
**	smin	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (min_s64_x_untied, svint64_t,
		z0 = svmin_s64_x (p0, z1, z2),
		z0 = svmin_x (p0, z1, z2))

/*
** min_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	smin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (min_x0_s64_x_tied1, svint64_t, int64_t,
		 z0 = svmin_n_s64_x (p0, z0, x0),
		 z0 = svmin_x (p0, z0, x0))

/*
** min_x0_s64_x_untied:
**	mov	z0\.d, x0
**	smin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (min_x0_s64_x_untied, svint64_t, int64_t,
		 z0 = svmin_n_s64_x (p0, z1, x0),
		 z0 = svmin_x (p0, z1, x0))

/*
** min_1_s64_x_tied1:
**	smin	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (min_1_s64_x_tied1, svint64_t,
		z0 = svmin_n_s64_x (p0, z0, 1),
		z0 = svmin_x (p0, z0, 1))

/*
** min_1_s64_x_untied:
**	movprfx	z0, z1
**	smin	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (min_1_s64_x_untied, svint64_t,
		z0 = svmin_n_s64_x (p0, z1, 1),
		z0 = svmin_x (p0, z1, 1))

/*
** min_127_s64_x:
**	smin	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (min_127_s64_x, svint64_t,
		z0 = svmin_n_s64_x (p0, z0, 127),
		z0 = svmin_x (p0, z0, 127))

/*
** min_128_s64_x:
**	mov	(z[0-9]+\.d), #128
**	smin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (min_128_s64_x, svint64_t,
		z0 = svmin_n_s64_x (p0, z0, 128),
		z0 = svmin_x (p0, z0, 128))

/*
** min_m1_s64_x:
**	smin	z0\.d, z0\.d, #-1
**	ret
*/
TEST_UNIFORM_Z (min_m1_s64_x, svint64_t,
		z0 = svmin_n_s64_x (p0, z0, -1),
		z0 = svmin_x (p0, z0, -1))

/*
** min_m128_s64_x:
**	smin	z0\.d, z0\.d, #-128
**	ret
*/
TEST_UNIFORM_Z (min_m128_s64_x, svint64_t,
		z0 = svmin_n_s64_x (p0, z0, -128),
		z0 = svmin_x (p0, z0, -128))

/*
** min_m129_s64_x:
**	mov	(z[0-9]+\.d), #-129
**	smin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (min_m129_s64_x, svint64_t,
		z0 = svmin_n_s64_x (p0, z0, -129),
		z0 = svmin_x (p0, z0, -129))
