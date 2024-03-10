/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsub_u16_tied1:
**	uqsub	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_u16_tied1, svuint16_t,
		z0 = svqsub_u16 (z0, z1),
		z0 = svqsub (z0, z1))

/*
** qsub_u16_tied2:
**	uqsub	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_u16_tied2, svuint16_t,
		z0 = svqsub_u16 (z1, z0),
		z0 = svqsub (z1, z0))

/*
** qsub_u16_untied:
**	uqsub	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_u16_untied, svuint16_t,
		z0 = svqsub_u16 (z1, z2),
		z0 = svqsub (z1, z2))

/*
** qsub_w0_u16_tied1:
**	mov	(z[0-9]+\.h), w0
**	uqsub	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u16_tied1, svuint16_t, uint16_t,
		 z0 = svqsub_n_u16 (z0, x0),
		 z0 = svqsub (z0, x0))

/*
** qsub_w0_u16_untied:
**	mov	(z[0-9]+\.h), w0
**	uqsub	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u16_untied, svuint16_t, uint16_t,
		 z0 = svqsub_n_u16 (z1, x0),
		 z0 = svqsub (z1, x0))

/*
** qsub_1_u16_tied1:
**	uqsub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u16_tied1, svuint16_t,
		z0 = svqsub_n_u16 (z0, 1),
		z0 = svqsub (z0, 1))

/*
** qsub_1_u16_untied:
**	movprfx	z0, z1
**	uqsub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u16_untied, svuint16_t,
		z0 = svqsub_n_u16 (z1, 1),
		z0 = svqsub (z1, 1))

/*
** qsub_127_u16:
**	uqsub	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u16, svuint16_t,
		z0 = svqsub_n_u16 (z0, 127),
		z0 = svqsub (z0, 127))

/*
** qsub_128_u16:
**	uqsub	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u16, svuint16_t,
		z0 = svqsub_n_u16 (z0, 128),
		z0 = svqsub (z0, 128))

/*
** qsub_255_u16:
**	uqsub	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u16, svuint16_t,
		z0 = svqsub_n_u16 (z0, 255),
		z0 = svqsub (z0, 255))

/*
** qsub_m1_u16:
**	mov	(z[0-9]+)\.b, #-1
**	uqsub	z0\.h, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u16, svuint16_t,
		z0 = svqsub_n_u16 (z0, -1),
		z0 = svqsub (z0, -1))

/*
** qsub_m127_u16:
**	mov	(z[0-9]+\.h), #-127
**	uqsub	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u16, svuint16_t,
		z0 = svqsub_n_u16 (z0, -127),
		z0 = svqsub (z0, -127))

/*
** qsub_m128_u16:
**	mov	(z[0-9]+\.h), #-128
**	uqsub	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u16, svuint16_t,
		z0 = svqsub_n_u16 (z0, -128),
		z0 = svqsub (z0, -128))

/*
** qsub_u16_m_tied1:
**	uqsub	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_u16_m_tied1, svuint16_t,
		z0 = svqsub_u16_m (p0, z0, z1),
		z0 = svqsub_m (p0, z0, z1))

/*
** qsub_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uqsub	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_u16_m_tied2, svuint16_t,
		z0 = svqsub_u16_m (p0, z1, z0),
		z0 = svqsub_m (p0, z1, z0))

/*
** qsub_u16_m_untied:
**	movprfx	z0, z1
**	uqsub	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_u16_m_untied, svuint16_t,
		z0 = svqsub_u16_m (p0, z1, z2),
		z0 = svqsub_m (p0, z1, z2))

/*
** qsub_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u16_m_tied1, svuint16_t, uint16_t,
		 z0 = svqsub_n_u16_m (p0, z0, x0),
		 z0 = svqsub_m (p0, z0, x0))

/*
** qsub_w0_u16_m_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u16_m_untied, svuint16_t, uint16_t,
		 z0 = svqsub_n_u16_m (p0, z1, x0),
		 z0 = svqsub_m (p0, z1, x0))

/*
** qsub_1_u16_m_tied1:
**	mov	(z[0-9]+\.h), #1
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u16_m_tied1, svuint16_t,
		z0 = svqsub_n_u16_m (p0, z0, 1),
		z0 = svqsub_m (p0, z0, 1))

/*
** qsub_1_u16_m_untied:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0, z1
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u16_m_untied, svuint16_t,
		z0 = svqsub_n_u16_m (p0, z1, 1),
		z0 = svqsub_m (p0, z1, 1))

/*
** qsub_127_u16_m:
**	mov	(z[0-9]+\.h), #127
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u16_m, svuint16_t,
		z0 = svqsub_n_u16_m (p0, z0, 127),
		z0 = svqsub_m (p0, z0, 127))

/*
** qsub_128_u16_m:
**	mov	(z[0-9]+\.h), #128
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u16_m, svuint16_t,
		z0 = svqsub_n_u16_m (p0, z0, 128),
		z0 = svqsub_m (p0, z0, 128))

/*
** qsub_255_u16_m:
**	mov	(z[0-9]+\.h), #255
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u16_m, svuint16_t,
		z0 = svqsub_n_u16_m (p0, z0, 255),
		z0 = svqsub_m (p0, z0, 255))

/*
** qsub_m1_u16_m:
**	mov	(z[0-9]+)\.b, #-1
**	uqsub	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u16_m, svuint16_t,
		z0 = svqsub_n_u16_m (p0, z0, -1),
		z0 = svqsub_m (p0, z0, -1))

/*
** qsub_m127_u16_m:
**	mov	(z[0-9]+\.h), #-127
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u16_m, svuint16_t,
		z0 = svqsub_n_u16_m (p0, z0, -127),
		z0 = svqsub_m (p0, z0, -127))

/*
** qsub_m128_u16_m:
**	mov	(z[0-9]+\.h), #-128
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u16_m, svuint16_t,
		z0 = svqsub_n_u16_m (p0, z0, -128),
		z0 = svqsub_m (p0, z0, -128))

/*
** qsub_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	uqsub	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_u16_z_tied1, svuint16_t,
		z0 = svqsub_u16_z (p0, z0, z1),
		z0 = svqsub_z (p0, z0, z1))

/*
** qsub_u16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	uqsubr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_u16_z_tied2, svuint16_t,
		z0 = svqsub_u16_z (p0, z1, z0),
		z0 = svqsub_z (p0, z1, z0))

/*
** qsub_u16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	uqsub	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	uqsubr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (qsub_u16_z_untied, svuint16_t,
		z0 = svqsub_u16_z (p0, z1, z2),
		z0 = svqsub_z (p0, z1, z2))

/*
** qsub_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u16_z_tied1, svuint16_t, uint16_t,
		 z0 = svqsub_n_u16_z (p0, z0, x0),
		 z0 = svqsub_z (p0, z0, x0))

/*
** qsub_w0_u16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	uqsub	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	uqsubr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u16_z_untied, svuint16_t, uint16_t,
		 z0 = svqsub_n_u16_z (p0, z1, x0),
		 z0 = svqsub_z (p0, z1, x0))

/*
** qsub_1_u16_z_tied1:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z0\.h
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u16_z_tied1, svuint16_t,
		z0 = svqsub_n_u16_z (p0, z0, 1),
		z0 = svqsub_z (p0, z0, 1))

/*
** qsub_1_u16_z_untied:
**	mov	(z[0-9]+\.h), #1
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	uqsub	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	uqsubr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u16_z_untied, svuint16_t,
		z0 = svqsub_n_u16_z (p0, z1, 1),
		z0 = svqsub_z (p0, z1, 1))

/*
** qsub_127_u16_z:
**	mov	(z[0-9]+\.h), #127
**	movprfx	z0\.h, p0/z, z0\.h
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u16_z, svuint16_t,
		z0 = svqsub_n_u16_z (p0, z0, 127),
		z0 = svqsub_z (p0, z0, 127))

/*
** qsub_128_u16_z:
**	mov	(z[0-9]+\.h), #128
**	movprfx	z0\.h, p0/z, z0\.h
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u16_z, svuint16_t,
		z0 = svqsub_n_u16_z (p0, z0, 128),
		z0 = svqsub_z (p0, z0, 128))

/*
** qsub_255_u16_z:
**	mov	(z[0-9]+\.h), #255
**	movprfx	z0\.h, p0/z, z0\.h
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u16_z, svuint16_t,
		z0 = svqsub_n_u16_z (p0, z0, 255),
		z0 = svqsub_z (p0, z0, 255))

/*
** qsub_m1_u16_z:
**	mov	(z[0-9]+)\.b, #-1
**	movprfx	z0\.h, p0/z, z0\.h
**	uqsub	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u16_z, svuint16_t,
		z0 = svqsub_n_u16_z (p0, z0, -1),
		z0 = svqsub_z (p0, z0, -1))

/*
** qsub_m127_u16_z:
**	mov	(z[0-9]+\.h), #-127
**	movprfx	z0\.h, p0/z, z0\.h
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u16_z, svuint16_t,
		z0 = svqsub_n_u16_z (p0, z0, -127),
		z0 = svqsub_z (p0, z0, -127))

/*
** qsub_m128_u16_z:
**	mov	(z[0-9]+\.h), #-128
**	movprfx	z0\.h, p0/z, z0\.h
**	uqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u16_z, svuint16_t,
		z0 = svqsub_n_u16_z (p0, z0, -128),
		z0 = svqsub_z (p0, z0, -128))

/*
** qsub_u16_x_tied1:
**	uqsub	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_u16_x_tied1, svuint16_t,
		z0 = svqsub_u16_x (p0, z0, z1),
		z0 = svqsub_x (p0, z0, z1))

/*
** qsub_u16_x_tied2:
**	uqsub	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_u16_x_tied2, svuint16_t,
		z0 = svqsub_u16_x (p0, z1, z0),
		z0 = svqsub_x (p0, z1, z0))

/*
** qsub_u16_x_untied:
**	uqsub	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_u16_x_untied, svuint16_t,
		z0 = svqsub_u16_x (p0, z1, z2),
		z0 = svqsub_x (p0, z1, z2))

/*
** qsub_w0_u16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	uqsub	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u16_x_tied1, svuint16_t, uint16_t,
		 z0 = svqsub_n_u16_x (p0, z0, x0),
		 z0 = svqsub_x (p0, z0, x0))

/*
** qsub_w0_u16_x_untied:
**	mov	(z[0-9]+\.h), w0
**	uqsub	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u16_x_untied, svuint16_t, uint16_t,
		 z0 = svqsub_n_u16_x (p0, z1, x0),
		 z0 = svqsub_x (p0, z1, x0))

/*
** qsub_1_u16_x_tied1:
**	uqsub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u16_x_tied1, svuint16_t,
		z0 = svqsub_n_u16_x (p0, z0, 1),
		z0 = svqsub_x (p0, z0, 1))

/*
** qsub_1_u16_x_untied:
**	movprfx	z0, z1
**	uqsub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u16_x_untied, svuint16_t,
		z0 = svqsub_n_u16_x (p0, z1, 1),
		z0 = svqsub_x (p0, z1, 1))

/*
** qsub_127_u16_x:
**	uqsub	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u16_x, svuint16_t,
		z0 = svqsub_n_u16_x (p0, z0, 127),
		z0 = svqsub_x (p0, z0, 127))

/*
** qsub_128_u16_x:
**	uqsub	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u16_x, svuint16_t,
		z0 = svqsub_n_u16_x (p0, z0, 128),
		z0 = svqsub_x (p0, z0, 128))

/*
** qsub_255_u16_x:
**	uqsub	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u16_x, svuint16_t,
		z0 = svqsub_n_u16_x (p0, z0, 255),
		z0 = svqsub_x (p0, z0, 255))

/*
** qsub_m1_u16_x:
**	mov	(z[0-9]+)\.b, #-1
**	uqsub	z0\.h, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u16_x, svuint16_t,
		z0 = svqsub_n_u16_x (p0, z0, -1),
		z0 = svqsub_x (p0, z0, -1))

/*
** qsub_m127_u16_x:
**	mov	(z[0-9]+\.h), #-127
**	uqsub	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u16_x, svuint16_t,
		z0 = svqsub_n_u16_x (p0, z0, -127),
		z0 = svqsub_x (p0, z0, -127))

/*
** qsub_m128_u16_x:
**	mov	(z[0-9]+\.h), #-128
**	uqsub	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u16_x, svuint16_t,
		z0 = svqsub_n_u16_x (p0, z0, -128),
		z0 = svqsub_x (p0, z0, -128))
