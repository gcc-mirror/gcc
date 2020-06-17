/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsub_u8_tied1:
**	uqsub	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_u8_tied1, svuint8_t,
		z0 = svqsub_u8 (z0, z1),
		z0 = svqsub (z0, z1))

/*
** qsub_u8_tied2:
**	uqsub	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_u8_tied2, svuint8_t,
		z0 = svqsub_u8 (z1, z0),
		z0 = svqsub (z1, z0))

/*
** qsub_u8_untied:
**	uqsub	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_u8_untied, svuint8_t,
		z0 = svqsub_u8 (z1, z2),
		z0 = svqsub (z1, z2))

/*
** qsub_w0_u8_tied1:
**	mov	(z[0-9]+\.b), w0
**	uqsub	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u8_tied1, svuint8_t, uint8_t,
		 z0 = svqsub_n_u8 (z0, x0),
		 z0 = svqsub (z0, x0))

/*
** qsub_w0_u8_untied:
**	mov	(z[0-9]+\.b), w0
**	uqsub	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u8_untied, svuint8_t, uint8_t,
		 z0 = svqsub_n_u8 (z1, x0),
		 z0 = svqsub (z1, x0))

/*
** qsub_1_u8_tied1:
**	uqsub	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u8_tied1, svuint8_t,
		z0 = svqsub_n_u8 (z0, 1),
		z0 = svqsub (z0, 1))

/*
** qsub_1_u8_untied:
**	movprfx	z0, z1
**	uqsub	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u8_untied, svuint8_t,
		z0 = svqsub_n_u8 (z1, 1),
		z0 = svqsub (z1, 1))

/*
** qsub_127_u8:
**	uqsub	z0\.b, z0\.b, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u8, svuint8_t,
		z0 = svqsub_n_u8 (z0, 127),
		z0 = svqsub (z0, 127))

/*
** qsub_128_u8:
**	uqsub	z0\.b, z0\.b, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u8, svuint8_t,
		z0 = svqsub_n_u8 (z0, 128),
		z0 = svqsub (z0, 128))

/*
** qsub_255_u8:
**	uqsub	z0\.b, z0\.b, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u8, svuint8_t,
		z0 = svqsub_n_u8 (z0, 255),
		z0 = svqsub (z0, 255))

/*
** qsub_m1_u8:
**	uqsub	z0\.b, z0\.b, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u8, svuint8_t,
		z0 = svqsub_n_u8 (z0, -1),
		z0 = svqsub (z0, -1))

/*
** qsub_m127_u8:
**	uqsub	z0\.b, z0\.b, #129
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u8, svuint8_t,
		z0 = svqsub_n_u8 (z0, -127),
		z0 = svqsub (z0, -127))

/*
** qsub_m128_u8:
**	uqsub	z0\.b, z0\.b, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u8, svuint8_t,
		z0 = svqsub_n_u8 (z0, -128),
		z0 = svqsub (z0, -128))

/*
** qsub_u8_m_tied1:
**	uqsub	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_u8_m_tied1, svuint8_t,
		z0 = svqsub_u8_m (p0, z0, z1),
		z0 = svqsub_m (p0, z0, z1))

/*
** qsub_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uqsub	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_u8_m_tied2, svuint8_t,
		z0 = svqsub_u8_m (p0, z1, z0),
		z0 = svqsub_m (p0, z1, z0))

/*
** qsub_u8_m_untied:
**	movprfx	z0, z1
**	uqsub	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_u8_m_untied, svuint8_t,
		z0 = svqsub_u8_m (p0, z1, z2),
		z0 = svqsub_m (p0, z1, z2))

/*
** qsub_w0_u8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u8_m_tied1, svuint8_t, uint8_t,
		 z0 = svqsub_n_u8_m (p0, z0, x0),
		 z0 = svqsub_m (p0, z0, x0))

/*
** qsub_w0_u8_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u8_m_untied, svuint8_t, uint8_t,
		 z0 = svqsub_n_u8_m (p0, z1, x0),
		 z0 = svqsub_m (p0, z1, x0))

/*
** qsub_1_u8_m_tied1:
**	mov	(z[0-9]+\.b), #1
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u8_m_tied1, svuint8_t,
		z0 = svqsub_n_u8_m (p0, z0, 1),
		z0 = svqsub_m (p0, z0, 1))

/*
** qsub_1_u8_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0, z1
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u8_m_untied, svuint8_t,
		z0 = svqsub_n_u8_m (p0, z1, 1),
		z0 = svqsub_m (p0, z1, 1))

/*
** qsub_127_u8_m:
**	mov	(z[0-9]+\.b), #127
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u8_m, svuint8_t,
		z0 = svqsub_n_u8_m (p0, z0, 127),
		z0 = svqsub_m (p0, z0, 127))

/*
** qsub_128_u8_m:
**	mov	(z[0-9]+\.b), #-128
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u8_m, svuint8_t,
		z0 = svqsub_n_u8_m (p0, z0, 128),
		z0 = svqsub_m (p0, z0, 128))

/*
** qsub_255_u8_m:
**	mov	(z[0-9]+\.b), #-1
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u8_m, svuint8_t,
		z0 = svqsub_n_u8_m (p0, z0, 255),
		z0 = svqsub_m (p0, z0, 255))

/*
** qsub_m1_u8_m:
**	mov	(z[0-9]+\.b), #-1
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u8_m, svuint8_t,
		z0 = svqsub_n_u8_m (p0, z0, -1),
		z0 = svqsub_m (p0, z0, -1))

/*
** qsub_m127_u8_m:
**	mov	(z[0-9]+\.b), #-127
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u8_m, svuint8_t,
		z0 = svqsub_n_u8_m (p0, z0, -127),
		z0 = svqsub_m (p0, z0, -127))

/*
** qsub_m128_u8_m:
**	mov	(z[0-9]+\.b), #-128
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u8_m, svuint8_t,
		z0 = svqsub_n_u8_m (p0, z0, -128),
		z0 = svqsub_m (p0, z0, -128))

/*
** qsub_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	uqsub	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_u8_z_tied1, svuint8_t,
		z0 = svqsub_u8_z (p0, z0, z1),
		z0 = svqsub_z (p0, z0, z1))

/*
** qsub_u8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	uqsubr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_u8_z_tied2, svuint8_t,
		z0 = svqsub_u8_z (p0, z1, z0),
		z0 = svqsub_z (p0, z1, z0))

/*
** qsub_u8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uqsub	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	uqsubr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (qsub_u8_z_untied, svuint8_t,
		z0 = svqsub_u8_z (p0, z1, z2),
		z0 = svqsub_z (p0, z1, z2))

/*
** qsub_w0_u8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u8_z_tied1, svuint8_t, uint8_t,
		 z0 = svqsub_n_u8_z (p0, z0, x0),
		 z0 = svqsub_z (p0, z0, x0))

/*
** qsub_w0_u8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uqsub	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	uqsubr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u8_z_untied, svuint8_t, uint8_t,
		 z0 = svqsub_n_u8_z (p0, z1, x0),
		 z0 = svqsub_z (p0, z1, x0))

/*
** qsub_1_u8_z_tied1:
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0\.b, p0/z, z0\.b
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u8_z_tied1, svuint8_t,
		z0 = svqsub_n_u8_z (p0, z0, 1),
		z0 = svqsub_z (p0, z0, 1))

/*
** qsub_1_u8_z_untied:
**	mov	(z[0-9]+\.b), #1
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	uqsub	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	uqsubr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u8_z_untied, svuint8_t,
		z0 = svqsub_n_u8_z (p0, z1, 1),
		z0 = svqsub_z (p0, z1, 1))

/*
** qsub_127_u8_z:
**	mov	(z[0-9]+\.b), #127
**	movprfx	z0\.b, p0/z, z0\.b
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u8_z, svuint8_t,
		z0 = svqsub_n_u8_z (p0, z0, 127),
		z0 = svqsub_z (p0, z0, 127))

/*
** qsub_128_u8_z:
**	mov	(z[0-9]+\.b), #-128
**	movprfx	z0\.b, p0/z, z0\.b
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u8_z, svuint8_t,
		z0 = svqsub_n_u8_z (p0, z0, 128),
		z0 = svqsub_z (p0, z0, 128))

/*
** qsub_255_u8_z:
**	mov	(z[0-9]+\.b), #-1
**	movprfx	z0\.b, p0/z, z0\.b
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u8_z, svuint8_t,
		z0 = svqsub_n_u8_z (p0, z0, 255),
		z0 = svqsub_z (p0, z0, 255))

/*
** qsub_m1_u8_z:
**	mov	(z[0-9]+\.b), #-1
**	movprfx	z0\.b, p0/z, z0\.b
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u8_z, svuint8_t,
		z0 = svqsub_n_u8_z (p0, z0, -1),
		z0 = svqsub_z (p0, z0, -1))

/*
** qsub_m127_u8_z:
**	mov	(z[0-9]+\.b), #-127
**	movprfx	z0\.b, p0/z, z0\.b
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u8_z, svuint8_t,
		z0 = svqsub_n_u8_z (p0, z0, -127),
		z0 = svqsub_z (p0, z0, -127))

/*
** qsub_m128_u8_z:
**	mov	(z[0-9]+\.b), #-128
**	movprfx	z0\.b, p0/z, z0\.b
**	uqsub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u8_z, svuint8_t,
		z0 = svqsub_n_u8_z (p0, z0, -128),
		z0 = svqsub_z (p0, z0, -128))

/*
** qsub_u8_x_tied1:
**	uqsub	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_u8_x_tied1, svuint8_t,
		z0 = svqsub_u8_x (p0, z0, z1),
		z0 = svqsub_x (p0, z0, z1))

/*
** qsub_u8_x_tied2:
**	uqsub	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_u8_x_tied2, svuint8_t,
		z0 = svqsub_u8_x (p0, z1, z0),
		z0 = svqsub_x (p0, z1, z0))

/*
** qsub_u8_x_untied:
**	uqsub	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_u8_x_untied, svuint8_t,
		z0 = svqsub_u8_x (p0, z1, z2),
		z0 = svqsub_x (p0, z1, z2))

/*
** qsub_w0_u8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	uqsub	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u8_x_tied1, svuint8_t, uint8_t,
		 z0 = svqsub_n_u8_x (p0, z0, x0),
		 z0 = svqsub_x (p0, z0, x0))

/*
** qsub_w0_u8_x_untied:
**	mov	(z[0-9]+\.b), w0
**	uqsub	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u8_x_untied, svuint8_t, uint8_t,
		 z0 = svqsub_n_u8_x (p0, z1, x0),
		 z0 = svqsub_x (p0, z1, x0))

/*
** qsub_1_u8_x_tied1:
**	uqsub	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u8_x_tied1, svuint8_t,
		z0 = svqsub_n_u8_x (p0, z0, 1),
		z0 = svqsub_x (p0, z0, 1))

/*
** qsub_1_u8_x_untied:
**	movprfx	z0, z1
**	uqsub	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u8_x_untied, svuint8_t,
		z0 = svqsub_n_u8_x (p0, z1, 1),
		z0 = svqsub_x (p0, z1, 1))

/*
** qsub_127_u8_x:
**	uqsub	z0\.b, z0\.b, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u8_x, svuint8_t,
		z0 = svqsub_n_u8_x (p0, z0, 127),
		z0 = svqsub_x (p0, z0, 127))

/*
** qsub_128_u8_x:
**	uqsub	z0\.b, z0\.b, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u8_x, svuint8_t,
		z0 = svqsub_n_u8_x (p0, z0, 128),
		z0 = svqsub_x (p0, z0, 128))

/*
** qsub_255_u8_x:
**	uqsub	z0\.b, z0\.b, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u8_x, svuint8_t,
		z0 = svqsub_n_u8_x (p0, z0, 255),
		z0 = svqsub_x (p0, z0, 255))

/*
** qsub_m1_u8_x:
**	uqsub	z0\.b, z0\.b, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u8_x, svuint8_t,
		z0 = svqsub_n_u8_x (p0, z0, -1),
		z0 = svqsub_x (p0, z0, -1))

/*
** qsub_m127_u8_x:
**	uqsub	z0\.b, z0\.b, #129
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u8_x, svuint8_t,
		z0 = svqsub_n_u8_x (p0, z0, -127),
		z0 = svqsub_x (p0, z0, -127))

/*
** qsub_m128_u8_x:
**	uqsub	z0\.b, z0\.b, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u8_x, svuint8_t,
		z0 = svqsub_n_u8_x (p0, z0, -128),
		z0 = svqsub_x (p0, z0, -128))
