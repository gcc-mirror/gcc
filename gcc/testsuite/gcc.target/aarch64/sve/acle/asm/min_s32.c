/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** min_s32_m_tied1:
**	smin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (min_s32_m_tied1, svint32_t,
		z0 = svmin_s32_m (p0, z0, z1),
		z0 = svmin_m (p0, z0, z1))

/*
** min_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	smin	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (min_s32_m_tied2, svint32_t,
		z0 = svmin_s32_m (p0, z1, z0),
		z0 = svmin_m (p0, z1, z0))

/*
** min_s32_m_untied:
**	movprfx	z0, z1
**	smin	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (min_s32_m_untied, svint32_t,
		z0 = svmin_s32_m (p0, z1, z2),
		z0 = svmin_m (p0, z1, z2))

/*
** min_w0_s32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	smin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_s32_m_tied1, svint32_t, int32_t,
		 z0 = svmin_n_s32_m (p0, z0, x0),
		 z0 = svmin_m (p0, z0, x0))

/*
** min_w0_s32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	smin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_s32_m_untied, svint32_t, int32_t,
		 z0 = svmin_n_s32_m (p0, z1, x0),
		 z0 = svmin_m (p0, z1, x0))

/*
** min_1_s32_m_tied1:
**	mov	(z[0-9]+\.s), #1
**	smin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_s32_m_tied1, svint32_t,
		z0 = svmin_n_s32_m (p0, z0, 1),
		z0 = svmin_m (p0, z0, 1))

/*
** min_1_s32_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0, z1
**	smin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_s32_m_untied, svint32_t,
		z0 = svmin_n_s32_m (p0, z1, 1),
		z0 = svmin_m (p0, z1, 1))

/*
** min_m1_s32_m:
**	mov	(z[0-9]+)\.b, #-1
**	smin	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (min_m1_s32_m, svint32_t,
		z0 = svmin_n_s32_m (p0, z0, -1),
		z0 = svmin_m (p0, z0, -1))

/*
** min_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	smin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (min_s32_z_tied1, svint32_t,
		z0 = svmin_s32_z (p0, z0, z1),
		z0 = svmin_z (p0, z0, z1))

/*
** min_s32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	smin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (min_s32_z_tied2, svint32_t,
		z0 = svmin_s32_z (p0, z1, z0),
		z0 = svmin_z (p0, z1, z0))

/*
** min_s32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	smin	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	smin	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (min_s32_z_untied, svint32_t,
		z0 = svmin_s32_z (p0, z1, z2),
		z0 = svmin_z (p0, z1, z2))

/*
** min_w0_s32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	smin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_s32_z_tied1, svint32_t, int32_t,
		 z0 = svmin_n_s32_z (p0, z0, x0),
		 z0 = svmin_z (p0, z0, x0))

/*
** min_w0_s32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	smin	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	smin	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (min_w0_s32_z_untied, svint32_t, int32_t,
		 z0 = svmin_n_s32_z (p0, z1, x0),
		 z0 = svmin_z (p0, z1, x0))

/*
** min_1_s32_z_tied1:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z0\.s
**	smin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_s32_z_tied1, svint32_t,
		z0 = svmin_n_s32_z (p0, z0, 1),
		z0 = svmin_z (p0, z0, 1))

/*
** min_1_s32_z_untied:
**	mov	(z[0-9]+\.s), #1
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	smin	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	smin	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (min_1_s32_z_untied, svint32_t,
		z0 = svmin_n_s32_z (p0, z1, 1),
		z0 = svmin_z (p0, z1, 1))

/*
** min_s32_x_tied1:
**	smin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (min_s32_x_tied1, svint32_t,
		z0 = svmin_s32_x (p0, z0, z1),
		z0 = svmin_x (p0, z0, z1))

/*
** min_s32_x_tied2:
**	smin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (min_s32_x_tied2, svint32_t,
		z0 = svmin_s32_x (p0, z1, z0),
		z0 = svmin_x (p0, z1, z0))

/*
** min_s32_x_untied:
** (
**	movprfx	z0, z1
**	smin	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	smin	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (min_s32_x_untied, svint32_t,
		z0 = svmin_s32_x (p0, z1, z2),
		z0 = svmin_x (p0, z1, z2))

/*
** min_w0_s32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	smin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_s32_x_tied1, svint32_t, int32_t,
		 z0 = svmin_n_s32_x (p0, z0, x0),
		 z0 = svmin_x (p0, z0, x0))

/*
** min_w0_s32_x_untied:
**	mov	z0\.s, w0
**	smin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (min_w0_s32_x_untied, svint32_t, int32_t,
		 z0 = svmin_n_s32_x (p0, z1, x0),
		 z0 = svmin_x (p0, z1, x0))

/*
** min_1_s32_x_tied1:
**	smin	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (min_1_s32_x_tied1, svint32_t,
		z0 = svmin_n_s32_x (p0, z0, 1),
		z0 = svmin_x (p0, z0, 1))

/*
** min_1_s32_x_untied:
**	movprfx	z0, z1
**	smin	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (min_1_s32_x_untied, svint32_t,
		z0 = svmin_n_s32_x (p0, z1, 1),
		z0 = svmin_x (p0, z1, 1))

/*
** min_127_s32_x:
**	smin	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (min_127_s32_x, svint32_t,
		z0 = svmin_n_s32_x (p0, z0, 127),
		z0 = svmin_x (p0, z0, 127))

/*
** min_128_s32_x:
**	mov	(z[0-9]+\.s), #128
**	smin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (min_128_s32_x, svint32_t,
		z0 = svmin_n_s32_x (p0, z0, 128),
		z0 = svmin_x (p0, z0, 128))

/*
** min_m1_s32_x:
**	smin	z0\.s, z0\.s, #-1
**	ret
*/
TEST_UNIFORM_Z (min_m1_s32_x, svint32_t,
		z0 = svmin_n_s32_x (p0, z0, -1),
		z0 = svmin_x (p0, z0, -1))

/*
** min_m128_s32_x:
**	smin	z0\.s, z0\.s, #-128
**	ret
*/
TEST_UNIFORM_Z (min_m128_s32_x, svint32_t,
		z0 = svmin_n_s32_x (p0, z0, -128),
		z0 = svmin_x (p0, z0, -128))

/*
** min_m129_s32_x:
**	mov	(z[0-9]+\.s), #-129
**	smin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (min_m129_s32_x, svint32_t,
		z0 = svmin_n_s32_x (p0, z0, -129),
		z0 = svmin_x (p0, z0, -129))
