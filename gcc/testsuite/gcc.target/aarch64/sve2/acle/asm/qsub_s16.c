/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsub_s16_tied1:
**	sqsub	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_tied1, svint16_t,
		z0 = svqsub_s16 (z0, z1),
		z0 = svqsub (z0, z1))

/*
** qsub_s16_tied2:
**	sqsub	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_tied2, svint16_t,
		z0 = svqsub_s16 (z1, z0),
		z0 = svqsub (z1, z0))

/*
** qsub_s16_untied:
**	sqsub	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_untied, svint16_t,
		z0 = svqsub_s16 (z1, z2),
		z0 = svqsub (z1, z2))

/*
** qsub_w0_s16_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqsub	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s16_tied1, svint16_t, int16_t,
		 z0 = svqsub_n_s16 (z0, x0),
		 z0 = svqsub (z0, x0))

/*
** qsub_w0_s16_untied:
**	mov	(z[0-9]+\.h), w0
**	sqsub	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s16_untied, svint16_t, int16_t,
		 z0 = svqsub_n_s16 (z1, x0),
		 z0 = svqsub (z1, x0))

/*
** qsub_1_s16_tied1:
**	sqsub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s16_tied1, svint16_t,
		z0 = svqsub_n_s16 (z0, 1),
		z0 = svqsub (z0, 1))

/*
** qsub_1_s16_untied:
**	movprfx	z0, z1
**	sqsub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s16_untied, svint16_t,
		z0 = svqsub_n_s16 (z1, 1),
		z0 = svqsub (z1, 1))

/*
** qsub_127_s16:
**	sqsub	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_s16, svint16_t,
		z0 = svqsub_n_s16 (z0, 127),
		z0 = svqsub (z0, 127))

/*
** qsub_128_s16:
**	sqsub	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_s16, svint16_t,
		z0 = svqsub_n_s16 (z0, 128),
		z0 = svqsub (z0, 128))

/*
** qsub_255_s16:
**	sqsub	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_s16, svint16_t,
		z0 = svqsub_n_s16 (z0, 255),
		z0 = svqsub (z0, 255))

/*
** qsub_m1_s16:
**	sqadd	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_s16, svint16_t,
		z0 = svqsub_n_s16 (z0, -1),
		z0 = svqsub (z0, -1))

/*
** qsub_m127_s16:
**	sqadd	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_s16, svint16_t,
		z0 = svqsub_n_s16 (z0, -127),
		z0 = svqsub (z0, -127))

/*
** qsub_m128_s16:
**	sqadd	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_s16, svint16_t,
		z0 = svqsub_n_s16 (z0, -128),
		z0 = svqsub (z0, -128))

/*
** qsub_s16_m_tied1:
**	sqsub	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_m_tied1, svint16_t,
		z0 = svqsub_s16_m (p0, z0, z1),
		z0 = svqsub_m (p0, z0, z1))

/*
** qsub_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqsub	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_m_tied2, svint16_t,
		z0 = svqsub_s16_m (p0, z1, z0),
		z0 = svqsub_m (p0, z1, z0))

/*
** qsub_s16_m_untied:
**	movprfx	z0, z1
**	sqsub	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_m_untied, svint16_t,
		z0 = svqsub_s16_m (p0, z1, z2),
		z0 = svqsub_m (p0, z1, z2))

/*
** qsub_w0_s16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s16_m_tied1, svint16_t, int16_t,
		 z0 = svqsub_n_s16_m (p0, z0, x0),
		 z0 = svqsub_m (p0, z0, x0))

/*
** qsub_w0_s16_m_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s16_m_untied, svint16_t, int16_t,
		 z0 = svqsub_n_s16_m (p0, z1, x0),
		 z0 = svqsub_m (p0, z1, x0))

/*
** qsub_1_s16_m_tied1:
**	mov	(z[0-9]+\.h), #1
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s16_m_tied1, svint16_t,
		z0 = svqsub_n_s16_m (p0, z0, 1),
		z0 = svqsub_m (p0, z0, 1))

/*
** qsub_1_s16_m_untied:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0, z1
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s16_m_untied, svint16_t,
		z0 = svqsub_n_s16_m (p0, z1, 1),
		z0 = svqsub_m (p0, z1, 1))

/*
** qsub_127_s16_m:
**	mov	(z[0-9]+\.h), #127
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_127_s16_m, svint16_t,
		z0 = svqsub_n_s16_m (p0, z0, 127),
		z0 = svqsub_m (p0, z0, 127))

/*
** qsub_128_s16_m:
**	mov	(z[0-9]+\.h), #128
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_128_s16_m, svint16_t,
		z0 = svqsub_n_s16_m (p0, z0, 128),
		z0 = svqsub_m (p0, z0, 128))

/*
** qsub_255_s16_m:
**	mov	(z[0-9]+\.h), #255
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_255_s16_m, svint16_t,
		z0 = svqsub_n_s16_m (p0, z0, 255),
		z0 = svqsub_m (p0, z0, 255))

/*
** qsub_m1_s16_m:
**	mov	(z[0-9]+)\.b, #-1
**	sqsub	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_s16_m, svint16_t,
		z0 = svqsub_n_s16_m (p0, z0, -1),
		z0 = svqsub_m (p0, z0, -1))

/*
** qsub_m127_s16_m:
**	mov	(z[0-9]+\.h), #-127
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_s16_m, svint16_t,
		z0 = svqsub_n_s16_m (p0, z0, -127),
		z0 = svqsub_m (p0, z0, -127))

/*
** qsub_m128_s16_m:
**	mov	(z[0-9]+\.h), #-128
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_s16_m, svint16_t,
		z0 = svqsub_n_s16_m (p0, z0, -128),
		z0 = svqsub_m (p0, z0, -128))

/*
** qsub_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqsub	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_z_tied1, svint16_t,
		z0 = svqsub_s16_z (p0, z0, z1),
		z0 = svqsub_z (p0, z0, z1))

/*
** qsub_s16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqsubr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_z_tied2, svint16_t,
		z0 = svqsub_s16_z (p0, z1, z0),
		z0 = svqsub_z (p0, z1, z0))

/*
** qsub_s16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	sqsub	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	sqsubr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_z_untied, svint16_t,
		z0 = svqsub_s16_z (p0, z1, z2),
		z0 = svqsub_z (p0, z1, z2))

/*
** qsub_w0_s16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s16_z_tied1, svint16_t, int16_t,
		 z0 = svqsub_n_s16_z (p0, z0, x0),
		 z0 = svqsub_z (p0, z0, x0))

/*
** qsub_w0_s16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	sqsub	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	sqsubr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s16_z_untied, svint16_t, int16_t,
		 z0 = svqsub_n_s16_z (p0, z1, x0),
		 z0 = svqsub_z (p0, z1, x0))

/*
** qsub_1_s16_z_tied1:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z0\.h
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s16_z_tied1, svint16_t,
		z0 = svqsub_n_s16_z (p0, z0, 1),
		z0 = svqsub_z (p0, z0, 1))

/*
** qsub_1_s16_z_untied:
**	mov	(z[0-9]+\.h), #1
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	sqsub	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	sqsubr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s16_z_untied, svint16_t,
		z0 = svqsub_n_s16_z (p0, z1, 1),
		z0 = svqsub_z (p0, z1, 1))

/*
** qsub_127_s16_z:
**	mov	(z[0-9]+\.h), #127
**	movprfx	z0\.h, p0/z, z0\.h
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_127_s16_z, svint16_t,
		z0 = svqsub_n_s16_z (p0, z0, 127),
		z0 = svqsub_z (p0, z0, 127))

/*
** qsub_128_s16_z:
**	mov	(z[0-9]+\.h), #128
**	movprfx	z0\.h, p0/z, z0\.h
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_128_s16_z, svint16_t,
		z0 = svqsub_n_s16_z (p0, z0, 128),
		z0 = svqsub_z (p0, z0, 128))

/*
** qsub_255_s16_z:
**	mov	(z[0-9]+\.h), #255
**	movprfx	z0\.h, p0/z, z0\.h
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_255_s16_z, svint16_t,
		z0 = svqsub_n_s16_z (p0, z0, 255),
		z0 = svqsub_z (p0, z0, 255))

/*
** qsub_m1_s16_z:
**	mov	(z[0-9]+)\.b, #-1
**	movprfx	z0\.h, p0/z, z0\.h
**	sqsub	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_s16_z, svint16_t,
		z0 = svqsub_n_s16_z (p0, z0, -1),
		z0 = svqsub_z (p0, z0, -1))

/*
** qsub_m127_s16_z:
**	mov	(z[0-9]+\.h), #-127
**	movprfx	z0\.h, p0/z, z0\.h
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_s16_z, svint16_t,
		z0 = svqsub_n_s16_z (p0, z0, -127),
		z0 = svqsub_z (p0, z0, -127))

/*
** qsub_m128_s16_z:
**	mov	(z[0-9]+\.h), #-128
**	movprfx	z0\.h, p0/z, z0\.h
**	sqsub	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_s16_z, svint16_t,
		z0 = svqsub_n_s16_z (p0, z0, -128),
		z0 = svqsub_z (p0, z0, -128))

/*
** qsub_s16_x_tied1:
**	sqsub	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_x_tied1, svint16_t,
		z0 = svqsub_s16_x (p0, z0, z1),
		z0 = svqsub_x (p0, z0, z1))

/*
** qsub_s16_x_tied2:
**	sqsub	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_x_tied2, svint16_t,
		z0 = svqsub_s16_x (p0, z1, z0),
		z0 = svqsub_x (p0, z1, z0))

/*
** qsub_s16_x_untied:
**	sqsub	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_x_untied, svint16_t,
		z0 = svqsub_s16_x (p0, z1, z2),
		z0 = svqsub_x (p0, z1, z2))

/*
** qsub_w0_s16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqsub	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s16_x_tied1, svint16_t, int16_t,
		 z0 = svqsub_n_s16_x (p0, z0, x0),
		 z0 = svqsub_x (p0, z0, x0))

/*
** qsub_w0_s16_x_untied:
**	mov	(z[0-9]+\.h), w0
**	sqsub	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s16_x_untied, svint16_t, int16_t,
		 z0 = svqsub_n_s16_x (p0, z1, x0),
		 z0 = svqsub_x (p0, z1, x0))

/*
** qsub_1_s16_x_tied1:
**	sqsub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s16_x_tied1, svint16_t,
		z0 = svqsub_n_s16_x (p0, z0, 1),
		z0 = svqsub_x (p0, z0, 1))

/*
** qsub_1_s16_x_untied:
**	movprfx	z0, z1
**	sqsub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s16_x_untied, svint16_t,
		z0 = svqsub_n_s16_x (p0, z1, 1),
		z0 = svqsub_x (p0, z1, 1))

/*
** qsub_127_s16_x:
**	sqsub	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_s16_x, svint16_t,
		z0 = svqsub_n_s16_x (p0, z0, 127),
		z0 = svqsub_x (p0, z0, 127))

/*
** qsub_128_s16_x:
**	sqsub	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_s16_x, svint16_t,
		z0 = svqsub_n_s16_x (p0, z0, 128),
		z0 = svqsub_x (p0, z0, 128))

/*
** qsub_255_s16_x:
**	sqsub	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_s16_x, svint16_t,
		z0 = svqsub_n_s16_x (p0, z0, 255),
		z0 = svqsub_x (p0, z0, 255))

/*
** qsub_m1_s16_x:
**	sqadd	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_s16_x, svint16_t,
		z0 = svqsub_n_s16_x (p0, z0, -1),
		z0 = svqsub_x (p0, z0, -1))

/*
** qsub_m127_s16_x:
**	sqadd	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_s16_x, svint16_t,
		z0 = svqsub_n_s16_x (p0, z0, -127),
		z0 = svqsub_x (p0, z0, -127))

/*
** qsub_m128_s16_x:
**	sqadd	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_s16_x, svint16_t,
		z0 = svqsub_n_s16_x (p0, z0, -128),
		z0 = svqsub_x (p0, z0, -128))
