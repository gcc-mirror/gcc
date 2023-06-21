/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** max_u16_m_tied1:
**	umax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (max_u16_m_tied1, svuint16_t,
		z0 = svmax_u16_m (p0, z0, z1),
		z0 = svmax_m (p0, z0, z1))

/*
** max_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	umax	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (max_u16_m_tied2, svuint16_t,
		z0 = svmax_u16_m (p0, z1, z0),
		z0 = svmax_m (p0, z1, z0))

/*
** max_u16_m_untied:
**	movprfx	z0, z1
**	umax	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (max_u16_m_untied, svuint16_t,
		z0 = svmax_u16_m (p0, z1, z2),
		z0 = svmax_m (p0, z1, z2))

/*
** max_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	umax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (max_w0_u16_m_tied1, svuint16_t, uint16_t,
		 z0 = svmax_n_u16_m (p0, z0, x0),
		 z0 = svmax_m (p0, z0, x0))

/*
** max_w0_u16_m_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	umax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (max_w0_u16_m_untied, svuint16_t, uint16_t,
		 z0 = svmax_n_u16_m (p0, z1, x0),
		 z0 = svmax_m (p0, z1, x0))

/*
** max_1_u16_m_tied1:
**	mov	(z[0-9]+\.h), #1
**	umax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (max_1_u16_m_tied1, svuint16_t,
		z0 = svmax_n_u16_m (p0, z0, 1),
		z0 = svmax_m (p0, z0, 1))

/*
** max_1_u16_m_untied:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0, z1
**	umax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (max_1_u16_m_untied, svuint16_t,
		z0 = svmax_n_u16_m (p0, z1, 1),
		z0 = svmax_m (p0, z1, 1))

/*
** max_m1_u16_m:
**	mov	(z[0-9]+)\.b, #-1
**	umax	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (max_m1_u16_m, svuint16_t,
		z0 = svmax_n_u16_m (p0, z0, -1),
		z0 = svmax_m (p0, z0, -1))

/*
** max_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	umax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (max_u16_z_tied1, svuint16_t,
		z0 = svmax_u16_z (p0, z0, z1),
		z0 = svmax_z (p0, z0, z1))

/*
** max_u16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	umax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (max_u16_z_tied2, svuint16_t,
		z0 = svmax_u16_z (p0, z1, z0),
		z0 = svmax_z (p0, z1, z0))

/*
** max_u16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	umax	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	umax	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (max_u16_z_untied, svuint16_t,
		z0 = svmax_u16_z (p0, z1, z2),
		z0 = svmax_z (p0, z1, z2))

/*
** max_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	umax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (max_w0_u16_z_tied1, svuint16_t, uint16_t,
		 z0 = svmax_n_u16_z (p0, z0, x0),
		 z0 = svmax_z (p0, z0, x0))

/*
** max_w0_u16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	umax	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	umax	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (max_w0_u16_z_untied, svuint16_t, uint16_t,
		 z0 = svmax_n_u16_z (p0, z1, x0),
		 z0 = svmax_z (p0, z1, x0))

/*
** max_1_u16_z_tied1:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z0\.h
**	umax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (max_1_u16_z_tied1, svuint16_t,
		z0 = svmax_n_u16_z (p0, z0, 1),
		z0 = svmax_z (p0, z0, 1))

/*
** max_1_u16_z_untied:
**	mov	(z[0-9]+\.h), #1
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	umax	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	umax	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (max_1_u16_z_untied, svuint16_t,
		z0 = svmax_n_u16_z (p0, z1, 1),
		z0 = svmax_z (p0, z1, 1))

/*
** max_u16_x_tied1:
**	umax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (max_u16_x_tied1, svuint16_t,
		z0 = svmax_u16_x (p0, z0, z1),
		z0 = svmax_x (p0, z0, z1))

/*
** max_u16_x_tied2:
**	umax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (max_u16_x_tied2, svuint16_t,
		z0 = svmax_u16_x (p0, z1, z0),
		z0 = svmax_x (p0, z1, z0))

/*
** max_u16_x_untied:
** (
**	movprfx	z0, z1
**	umax	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0, z2
**	umax	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (max_u16_x_untied, svuint16_t,
		z0 = svmax_u16_x (p0, z1, z2),
		z0 = svmax_x (p0, z1, z2))

/*
** max_w0_u16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	umax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (max_w0_u16_x_tied1, svuint16_t, uint16_t,
		 z0 = svmax_n_u16_x (p0, z0, x0),
		 z0 = svmax_x (p0, z0, x0))

/*
** max_w0_u16_x_untied:
**	mov	z0\.h, w0
**	umax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (max_w0_u16_x_untied, svuint16_t, uint16_t,
		 z0 = svmax_n_u16_x (p0, z1, x0),
		 z0 = svmax_x (p0, z1, x0))

/*
** max_1_u16_x_tied1:
**	umax	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (max_1_u16_x_tied1, svuint16_t,
		z0 = svmax_n_u16_x (p0, z0, 1),
		z0 = svmax_x (p0, z0, 1))

/*
** max_1_u16_x_untied:
**	movprfx	z0, z1
**	umax	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (max_1_u16_x_untied, svuint16_t,
		z0 = svmax_n_u16_x (p0, z1, 1),
		z0 = svmax_x (p0, z1, 1))

/*
** max_127_u16_x:
**	umax	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (max_127_u16_x, svuint16_t,
		z0 = svmax_n_u16_x (p0, z0, 127),
		z0 = svmax_x (p0, z0, 127))

/*
** max_128_u16_x:
**	umax	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (max_128_u16_x, svuint16_t,
		z0 = svmax_n_u16_x (p0, z0, 128),
		z0 = svmax_x (p0, z0, 128))

/*
** max_255_u16_x:
**	umax	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (max_255_u16_x, svuint16_t,
		z0 = svmax_n_u16_x (p0, z0, 255),
		z0 = svmax_x (p0, z0, 255))

/*
** max_256_u16_x:
**	mov	(z[0-9]+\.h), #256
**	umax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (max_256_u16_x, svuint16_t,
		z0 = svmax_n_u16_x (p0, z0, 256),
		z0 = svmax_x (p0, z0, 256))

/*
** max_m2_u16_x:
**	mov	(z[0-9]+\.h), #-2
**	umax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (max_m2_u16_x, svuint16_t,
		z0 = svmax_n_u16_x (p0, z0, -2),
		z0 = svmax_x (p0, z0, -2))
