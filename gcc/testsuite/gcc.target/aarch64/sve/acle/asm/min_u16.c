/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** min_u16_m_tied1:
**	umin	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (min_u16_m_tied1, svuint16_t,
		z0 = svmin_u16_m (p0, z0, z1),
		z0 = svmin_m (p0, z0, z1))

/*
** min_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	umin	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (min_u16_m_tied2, svuint16_t,
		z0 = svmin_u16_m (p0, z1, z0),
		z0 = svmin_m (p0, z1, z0))

/*
** min_u16_m_untied:
**	movprfx	z0, z1
**	umin	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (min_u16_m_untied, svuint16_t,
		z0 = svmin_u16_m (p0, z1, z2),
		z0 = svmin_m (p0, z1, z2))

/*
** min_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	umin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u16_m_tied1, svuint16_t, uint16_t,
		 z0 = svmin_n_u16_m (p0, z0, x0),
		 z0 = svmin_m (p0, z0, x0))

/*
** min_w0_u16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	umin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u16_m_untied, svuint16_t, uint16_t,
		 z0 = svmin_n_u16_m (p0, z1, x0),
		 z0 = svmin_m (p0, z1, x0))

/*
** min_1_u16_m_tied1:
**	mov	(z[0-9]+\.h), #1
**	umin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_u16_m_tied1, svuint16_t,
		z0 = svmin_n_u16_m (p0, z0, 1),
		z0 = svmin_m (p0, z0, 1))

/*
** min_1_u16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0, z1
**	umin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_u16_m_untied, svuint16_t,
		z0 = svmin_n_u16_m (p0, z1, 1),
		z0 = svmin_m (p0, z1, 1))

/*
** min_m1_u16_m:
**	mov	(z[0-9]+)\.b, #-1
**	umin	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (min_m1_u16_m, svuint16_t,
		z0 = svmin_n_u16_m (p0, z0, -1),
		z0 = svmin_m (p0, z0, -1))

/*
** min_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	umin	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (min_u16_z_tied1, svuint16_t,
		z0 = svmin_u16_z (p0, z0, z1),
		z0 = svmin_z (p0, z0, z1))

/*
** min_u16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	umin	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (min_u16_z_tied2, svuint16_t,
		z0 = svmin_u16_z (p0, z1, z0),
		z0 = svmin_z (p0, z1, z0))

/*
** min_u16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	umin	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	umin	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (min_u16_z_untied, svuint16_t,
		z0 = svmin_u16_z (p0, z1, z2),
		z0 = svmin_z (p0, z1, z2))

/*
** min_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	umin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u16_z_tied1, svuint16_t, uint16_t,
		 z0 = svmin_n_u16_z (p0, z0, x0),
		 z0 = svmin_z (p0, z0, x0))

/*
** min_w0_u16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	umin	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	umin	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u16_z_untied, svuint16_t, uint16_t,
		 z0 = svmin_n_u16_z (p0, z1, x0),
		 z0 = svmin_z (p0, z1, x0))

/*
** min_1_u16_z_tied1:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z0\.h
**	umin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (min_1_u16_z_tied1, svuint16_t,
		z0 = svmin_n_u16_z (p0, z0, 1),
		z0 = svmin_z (p0, z0, 1))

/*
** min_1_u16_z_untied:
**	mov	(z[0-9]+\.h), #1
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	umin	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	umin	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (min_1_u16_z_untied, svuint16_t,
		z0 = svmin_n_u16_z (p0, z1, 1),
		z0 = svmin_z (p0, z1, 1))

/*
** min_u16_x_tied1:
**	umin	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (min_u16_x_tied1, svuint16_t,
		z0 = svmin_u16_x (p0, z0, z1),
		z0 = svmin_x (p0, z0, z1))

/*
** min_u16_x_tied2:
**	umin	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (min_u16_x_tied2, svuint16_t,
		z0 = svmin_u16_x (p0, z1, z0),
		z0 = svmin_x (p0, z1, z0))

/*
** min_u16_x_untied:
** (
**	movprfx	z0, z1
**	umin	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0, z2
**	umin	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (min_u16_x_untied, svuint16_t,
		z0 = svmin_u16_x (p0, z1, z2),
		z0 = svmin_x (p0, z1, z2))

/*
** min_w0_u16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	umin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u16_x_tied1, svuint16_t, uint16_t,
		 z0 = svmin_n_u16_x (p0, z0, x0),
		 z0 = svmin_x (p0, z0, x0))

/*
** min_w0_u16_x_untied:
**	mov	z0\.h, w0
**	umin	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (min_w0_u16_x_untied, svuint16_t, uint16_t,
		 z0 = svmin_n_u16_x (p0, z1, x0),
		 z0 = svmin_x (p0, z1, x0))

/*
** min_1_u16_x_tied1:
**	umin	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (min_1_u16_x_tied1, svuint16_t,
		z0 = svmin_n_u16_x (p0, z0, 1),
		z0 = svmin_x (p0, z0, 1))

/*
** min_1_u16_x_untied:
**	movprfx	z0, z1
**	umin	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (min_1_u16_x_untied, svuint16_t,
		z0 = svmin_n_u16_x (p0, z1, 1),
		z0 = svmin_x (p0, z1, 1))

/*
** min_127_u16_x:
**	umin	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (min_127_u16_x, svuint16_t,
		z0 = svmin_n_u16_x (p0, z0, 127),
		z0 = svmin_x (p0, z0, 127))

/*
** min_128_u16_x:
**	umin	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (min_128_u16_x, svuint16_t,
		z0 = svmin_n_u16_x (p0, z0, 128),
		z0 = svmin_x (p0, z0, 128))

/*
** min_255_u16_x:
**	umin	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (min_255_u16_x, svuint16_t,
		z0 = svmin_n_u16_x (p0, z0, 255),
		z0 = svmin_x (p0, z0, 255))

/*
** min_256_u16_x:
**	mov	(z[0-9]+\.h), #256
**	umin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (min_256_u16_x, svuint16_t,
		z0 = svmin_n_u16_x (p0, z0, 256),
		z0 = svmin_x (p0, z0, 256))

/*
** min_m2_u16_x:
**	mov	(z[0-9]+\.h), #-2
**	umin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (min_m2_u16_x, svuint16_t,
		z0 = svmin_n_u16_x (p0, z0, -2),
		z0 = svmin_x (p0, z0, -2))
