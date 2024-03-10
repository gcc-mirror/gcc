/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** min_u32_m_tied1:
**	umin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (min_u32_m_tied1, svuint32_t,
		z0 = svmin_u32_m (p0, z0, z1),
		z0 = svmin_m (p0, z0, z1))

/*
** min_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	umin	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (min_u32_m_tied2, svuint32_t,
		z0 = svmin_u32_m (p0, z1, z0),
		z0 = svmin_m (p0, z1, z0))

/*
** min_u32_m_untied:
**	movprfx	z0, z1
**	umin	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (min_u32_m_untied, svuint32_t,
		z0 = svmin_u32_m (p0, z1, z2),
		z0 = svmin_m (p0, z1, z2))

/*
** min_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	umin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u32_m_tied1, svuint32_t, uint32_t,
		 z0 = svmin_n_u32_m (p0, z0, x0),
		 z0 = svmin_m (p0, z0, x0))

/*
** min_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	umin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u32_m_untied, svuint32_t, uint32_t,
		 z0 = svmin_n_u32_m (p0, z1, x0),
		 z0 = svmin_m (p0, z1, x0))

/*
** min_1_u32_m_tied1:
**	mov	(z[0-9]+\.s), #1
**	umin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_u32_m_tied1, svuint32_t,
		z0 = svmin_n_u32_m (p0, z0, 1),
		z0 = svmin_m (p0, z0, 1))

/*
** min_1_u32_m_untied:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0, z1
**	umin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_u32_m_untied, svuint32_t,
		z0 = svmin_n_u32_m (p0, z1, 1),
		z0 = svmin_m (p0, z1, 1))

/*
** min_m1_u32_m:
**	mov	(z[0-9]+)\.b, #-1
**	umin	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (min_m1_u32_m, svuint32_t,
		z0 = svmin_n_u32_m (p0, z0, -1),
		z0 = svmin_m (p0, z0, -1))

/*
** min_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	umin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (min_u32_z_tied1, svuint32_t,
		z0 = svmin_u32_z (p0, z0, z1),
		z0 = svmin_z (p0, z0, z1))

/*
** min_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	umin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (min_u32_z_tied2, svuint32_t,
		z0 = svmin_u32_z (p0, z1, z0),
		z0 = svmin_z (p0, z1, z0))

/*
** min_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	umin	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	umin	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (min_u32_z_untied, svuint32_t,
		z0 = svmin_u32_z (p0, z1, z2),
		z0 = svmin_z (p0, z1, z2))

/*
** min_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	umin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u32_z_tied1, svuint32_t, uint32_t,
		 z0 = svmin_n_u32_z (p0, z0, x0),
		 z0 = svmin_z (p0, z0, x0))

/*
** min_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	umin	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	umin	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u32_z_untied, svuint32_t, uint32_t,
		 z0 = svmin_n_u32_z (p0, z1, x0),
		 z0 = svmin_z (p0, z1, x0))

/*
** min_1_u32_z_tied1:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z0\.s
**	umin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_u32_z_tied1, svuint32_t,
		z0 = svmin_n_u32_z (p0, z0, 1),
		z0 = svmin_z (p0, z0, 1))

/*
** min_1_u32_z_untied:
**	mov	(z[0-9]+\.s), #1
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	umin	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	umin	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (min_1_u32_z_untied, svuint32_t,
		z0 = svmin_n_u32_z (p0, z1, 1),
		z0 = svmin_z (p0, z1, 1))

/*
** min_u32_x_tied1:
**	umin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (min_u32_x_tied1, svuint32_t,
		z0 = svmin_u32_x (p0, z0, z1),
		z0 = svmin_x (p0, z0, z1))

/*
** min_u32_x_tied2:
**	umin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (min_u32_x_tied2, svuint32_t,
		z0 = svmin_u32_x (p0, z1, z0),
		z0 = svmin_x (p0, z1, z0))

/*
** min_u32_x_untied:
** (
**	movprfx	z0, z1
**	umin	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	umin	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (min_u32_x_untied, svuint32_t,
		z0 = svmin_u32_x (p0, z1, z2),
		z0 = svmin_x (p0, z1, z2))

/*
** min_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	umin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u32_x_tied1, svuint32_t, uint32_t,
		 z0 = svmin_n_u32_x (p0, z0, x0),
		 z0 = svmin_x (p0, z0, x0))

/*
** min_w0_u32_x_untied:
**	mov	z0\.s, w0
**	umin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u32_x_untied, svuint32_t, uint32_t,
		 z0 = svmin_n_u32_x (p0, z1, x0),
		 z0 = svmin_x (p0, z1, x0))

/*
** min_1_u32_x_tied1:
**	umin	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (min_1_u32_x_tied1, svuint32_t,
		z0 = svmin_n_u32_x (p0, z0, 1),
		z0 = svmin_x (p0, z0, 1))

/*
** min_1_u32_x_untied:
**	movprfx	z0, z1
**	umin	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (min_1_u32_x_untied, svuint32_t,
		z0 = svmin_n_u32_x (p0, z1, 1),
		z0 = svmin_x (p0, z1, 1))

/*
** min_127_u32_x:
**	umin	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (min_127_u32_x, svuint32_t,
		z0 = svmin_n_u32_x (p0, z0, 127),
		z0 = svmin_x (p0, z0, 127))

/*
** min_128_u32_x:
**	umin	z0\.s, z0\.s, #128
**	ret
*/
TEST_UNIFORM_Z (min_128_u32_x, svuint32_t,
		z0 = svmin_n_u32_x (p0, z0, 128),
		z0 = svmin_x (p0, z0, 128))

/*
** min_255_u32_x:
**	umin	z0\.s, z0\.s, #255
**	ret
*/
TEST_UNIFORM_Z (min_255_u32_x, svuint32_t,
		z0 = svmin_n_u32_x (p0, z0, 255),
		z0 = svmin_x (p0, z0, 255))

/*
** min_256_u32_x:
**	mov	(z[0-9]+\.s), #256
**	umin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (min_256_u32_x, svuint32_t,
		z0 = svmin_n_u32_x (p0, z0, 256),
		z0 = svmin_x (p0, z0, 256))

/*
** min_m2_u32_x:
**	mov	(z[0-9]+\.s), #-2
**	umin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (min_m2_u32_x, svuint32_t,
		z0 = svmin_n_u32_x (p0, z0, -2),
		z0 = svmin_x (p0, z0, -2))
