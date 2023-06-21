/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** min_u8_m_tied1:
**	umin	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (min_u8_m_tied1, svuint8_t,
		z0 = svmin_u8_m (p0, z0, z1),
		z0 = svmin_m (p0, z0, z1))

/*
** min_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	umin	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (min_u8_m_tied2, svuint8_t,
		z0 = svmin_u8_m (p0, z1, z0),
		z0 = svmin_m (p0, z1, z0))

/*
** min_u8_m_untied:
**	movprfx	z0, z1
**	umin	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (min_u8_m_untied, svuint8_t,
		z0 = svmin_u8_m (p0, z1, z2),
		z0 = svmin_m (p0, z1, z2))

/*
** min_w0_u8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	umin	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u8_m_tied1, svuint8_t, uint8_t,
		 z0 = svmin_n_u8_m (p0, z0, x0),
		 z0 = svmin_m (p0, z0, x0))

/*
** min_w0_u8_m_untied:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	umin	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u8_m_untied, svuint8_t, uint8_t,
		 z0 = svmin_n_u8_m (p0, z1, x0),
		 z0 = svmin_m (p0, z1, x0))

/*
** min_1_u8_m_tied1:
**	mov	(z[0-9]+\.b), #1
**	umin	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_u8_m_tied1, svuint8_t,
		z0 = svmin_n_u8_m (p0, z0, 1),
		z0 = svmin_m (p0, z0, 1))

/*
** min_1_u8_m_untied:
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0, z1
**	umin	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_u8_m_untied, svuint8_t,
		z0 = svmin_n_u8_m (p0, z1, 1),
		z0 = svmin_m (p0, z1, 1))

/*
** min_m1_u8_m:
**	mov	(z[0-9]+\.b), #-1
**	umin	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (min_m1_u8_m, svuint8_t,
		z0 = svmin_n_u8_m (p0, z0, -1),
		z0 = svmin_m (p0, z0, -1))

/*
** min_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	umin	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (min_u8_z_tied1, svuint8_t,
		z0 = svmin_u8_z (p0, z0, z1),
		z0 = svmin_z (p0, z0, z1))

/*
** min_u8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	umin	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (min_u8_z_tied2, svuint8_t,
		z0 = svmin_u8_z (p0, z1, z0),
		z0 = svmin_z (p0, z1, z0))

/*
** min_u8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	umin	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	umin	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (min_u8_z_untied, svuint8_t,
		z0 = svmin_u8_z (p0, z1, z2),
		z0 = svmin_z (p0, z1, z2))

/*
** min_w0_u8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	umin	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u8_z_tied1, svuint8_t, uint8_t,
		 z0 = svmin_n_u8_z (p0, z0, x0),
		 z0 = svmin_z (p0, z0, x0))

/*
** min_w0_u8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	umin	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	umin	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u8_z_untied, svuint8_t, uint8_t,
		 z0 = svmin_n_u8_z (p0, z1, x0),
		 z0 = svmin_z (p0, z1, x0))

/*
** min_1_u8_z_tied1:
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0\.b, p0/z, z0\.b
**	umin	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_u8_z_tied1, svuint8_t,
		z0 = svmin_n_u8_z (p0, z0, 1),
		z0 = svmin_z (p0, z0, 1))

/*
** min_1_u8_z_untied:
**	mov	(z[0-9]+\.b), #1
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	umin	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	umin	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (min_1_u8_z_untied, svuint8_t,
		z0 = svmin_n_u8_z (p0, z1, 1),
		z0 = svmin_z (p0, z1, 1))

/*
** min_u8_x_tied1:
**	umin	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (min_u8_x_tied1, svuint8_t,
		z0 = svmin_u8_x (p0, z0, z1),
		z0 = svmin_x (p0, z0, z1))

/*
** min_u8_x_tied2:
**	umin	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (min_u8_x_tied2, svuint8_t,
		z0 = svmin_u8_x (p0, z1, z0),
		z0 = svmin_x (p0, z1, z0))

/*
** min_u8_x_untied:
** (
**	movprfx	z0, z1
**	umin	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0, z2
**	umin	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (min_u8_x_untied, svuint8_t,
		z0 = svmin_u8_x (p0, z1, z2),
		z0 = svmin_x (p0, z1, z2))

/*
** min_w0_u8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	umin	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u8_x_tied1, svuint8_t, uint8_t,
		 z0 = svmin_n_u8_x (p0, z0, x0),
		 z0 = svmin_x (p0, z0, x0))

/*
** min_w0_u8_x_untied:
**	mov	z0\.b, w0
**	umin	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u8_x_untied, svuint8_t, uint8_t,
		 z0 = svmin_n_u8_x (p0, z1, x0),
		 z0 = svmin_x (p0, z1, x0))

/*
** min_1_u8_x_tied1:
**	umin	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (min_1_u8_x_tied1, svuint8_t,
		z0 = svmin_n_u8_x (p0, z0, 1),
		z0 = svmin_x (p0, z0, 1))

/*
** min_1_u8_x_untied:
**	movprfx	z0, z1
**	umin	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (min_1_u8_x_untied, svuint8_t,
		z0 = svmin_n_u8_x (p0, z1, 1),
		z0 = svmin_x (p0, z1, 1))

/*
** min_127_u8_x:
**	umin	z0\.b, z0\.b, #127
**	ret
*/
TEST_UNIFORM_Z (min_127_u8_x, svuint8_t,
		z0 = svmin_n_u8_x (p0, z0, 127),
		z0 = svmin_x (p0, z0, 127))

/*
** min_128_u8_x:
**	umin	z0\.b, z0\.b, #128
**	ret
*/
TEST_UNIFORM_Z (min_128_u8_x, svuint8_t,
		z0 = svmin_n_u8_x (p0, z0, 128),
		z0 = svmin_x (p0, z0, 128))

/*
** min_254_u8_x:
**	umin	z0\.b, z0\.b, #254
**	ret
*/
TEST_UNIFORM_Z (min_254_u8_x, svuint8_t,
		z0 = svmin_n_u8_x (p0, z0, 254),
		z0 = svmin_x (p0, z0, 254))
