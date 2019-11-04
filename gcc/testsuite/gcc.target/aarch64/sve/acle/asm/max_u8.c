/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** max_u8_m_tied1:
**	umax	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (max_u8_m_tied1, svuint8_t,
		z0 = svmax_u8_m (p0, z0, z1),
		z0 = svmax_m (p0, z0, z1))

/*
** max_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	umax	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (max_u8_m_tied2, svuint8_t,
		z0 = svmax_u8_m (p0, z1, z0),
		z0 = svmax_m (p0, z1, z0))

/*
** max_u8_m_untied:
**	movprfx	z0, z1
**	umax	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (max_u8_m_untied, svuint8_t,
		z0 = svmax_u8_m (p0, z1, z2),
		z0 = svmax_m (p0, z1, z2))

/*
** max_w0_u8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	umax	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (max_w0_u8_m_tied1, svuint8_t, uint8_t,
		 z0 = svmax_n_u8_m (p0, z0, x0),
		 z0 = svmax_m (p0, z0, x0))

/*
** max_w0_u8_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	umax	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (max_w0_u8_m_untied, svuint8_t, uint8_t,
		 z0 = svmax_n_u8_m (p0, z1, x0),
		 z0 = svmax_m (p0, z1, x0))

/*
** max_1_u8_m_tied1:
**	mov	(z[0-9]+\.b), #1
**	umax	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (max_1_u8_m_tied1, svuint8_t,
		z0 = svmax_n_u8_m (p0, z0, 1),
		z0 = svmax_m (p0, z0, 1))

/*
** max_1_u8_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0, z1
**	umax	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (max_1_u8_m_untied, svuint8_t,
		z0 = svmax_n_u8_m (p0, z1, 1),
		z0 = svmax_m (p0, z1, 1))

/*
** max_m1_u8_m:
**	mov	(z[0-9]+\.b), #-1
**	umax	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (max_m1_u8_m, svuint8_t,
		z0 = svmax_n_u8_m (p0, z0, -1),
		z0 = svmax_m (p0, z0, -1))

/*
** max_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	umax	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (max_u8_z_tied1, svuint8_t,
		z0 = svmax_u8_z (p0, z0, z1),
		z0 = svmax_z (p0, z0, z1))

/*
** max_u8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	umax	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (max_u8_z_tied2, svuint8_t,
		z0 = svmax_u8_z (p0, z1, z0),
		z0 = svmax_z (p0, z1, z0))

/*
** max_u8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	umax	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	umax	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (max_u8_z_untied, svuint8_t,
		z0 = svmax_u8_z (p0, z1, z2),
		z0 = svmax_z (p0, z1, z2))

/*
** max_w0_u8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	umax	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (max_w0_u8_z_tied1, svuint8_t, uint8_t,
		 z0 = svmax_n_u8_z (p0, z0, x0),
		 z0 = svmax_z (p0, z0, x0))

/*
** max_w0_u8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	umax	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	umax	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (max_w0_u8_z_untied, svuint8_t, uint8_t,
		 z0 = svmax_n_u8_z (p0, z1, x0),
		 z0 = svmax_z (p0, z1, x0))

/*
** max_1_u8_z_tied1:
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0\.b, p0/z, z0\.b
**	umax	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (max_1_u8_z_tied1, svuint8_t,
		z0 = svmax_n_u8_z (p0, z0, 1),
		z0 = svmax_z (p0, z0, 1))

/*
** max_1_u8_z_untied:
**	mov	(z[0-9]+\.b), #1
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	umax	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	umax	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (max_1_u8_z_untied, svuint8_t,
		z0 = svmax_n_u8_z (p0, z1, 1),
		z0 = svmax_z (p0, z1, 1))

/*
** max_u8_x_tied1:
**	umax	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (max_u8_x_tied1, svuint8_t,
		z0 = svmax_u8_x (p0, z0, z1),
		z0 = svmax_x (p0, z0, z1))

/*
** max_u8_x_tied2:
**	umax	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (max_u8_x_tied2, svuint8_t,
		z0 = svmax_u8_x (p0, z1, z0),
		z0 = svmax_x (p0, z1, z0))

/*
** max_u8_x_untied:
** (
**	movprfx	z0, z1
**	umax	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0, z2
**	umax	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (max_u8_x_untied, svuint8_t,
		z0 = svmax_u8_x (p0, z1, z2),
		z0 = svmax_x (p0, z1, z2))

/*
** max_w0_u8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	umax	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (max_w0_u8_x_tied1, svuint8_t, uint8_t,
		 z0 = svmax_n_u8_x (p0, z0, x0),
		 z0 = svmax_x (p0, z0, x0))

/*
** max_w0_u8_x_untied:
**	mov	z0\.b, w0
**	umax	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (max_w0_u8_x_untied, svuint8_t, uint8_t,
		 z0 = svmax_n_u8_x (p0, z1, x0),
		 z0 = svmax_x (p0, z1, x0))

/*
** max_1_u8_x_tied1:
**	umax	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (max_1_u8_x_tied1, svuint8_t,
		z0 = svmax_n_u8_x (p0, z0, 1),
		z0 = svmax_x (p0, z0, 1))

/*
** max_1_u8_x_untied:
**	movprfx	z0, z1
**	umax	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (max_1_u8_x_untied, svuint8_t,
		z0 = svmax_n_u8_x (p0, z1, 1),
		z0 = svmax_x (p0, z1, 1))

/*
** max_127_u8_x:
**	umax	z0\.b, z0\.b, #127
**	ret
*/
TEST_UNIFORM_Z (max_127_u8_x, svuint8_t,
		z0 = svmax_n_u8_x (p0, z0, 127),
		z0 = svmax_x (p0, z0, 127))

/*
** max_128_u8_x:
**	umax	z0\.b, z0\.b, #128
**	ret
*/
TEST_UNIFORM_Z (max_128_u8_x, svuint8_t,
		z0 = svmax_n_u8_x (p0, z0, 128),
		z0 = svmax_x (p0, z0, 128))

/*
** max_254_u8_x:
**	umax	z0\.b, z0\.b, #254
**	ret
*/
TEST_UNIFORM_Z (max_254_u8_x, svuint8_t,
		z0 = svmax_n_u8_x (p0, z0, 254),
		z0 = svmax_x (p0, z0, 254))
