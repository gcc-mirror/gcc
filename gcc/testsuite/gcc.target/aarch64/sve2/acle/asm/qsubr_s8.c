/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsubr_s8_m_tied1:
**	sqsubr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_s8_m_tied1, svint8_t,
		z0 = svqsubr_s8_m (p0, z0, z1),
		z0 = svqsubr_m (p0, z0, z1))

/*
** qsubr_s8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqsubr	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_s8_m_tied2, svint8_t,
		z0 = svqsubr_s8_m (p0, z1, z0),
		z0 = svqsubr_m (p0, z1, z0))

/*
** qsubr_s8_m_untied:
**	movprfx	z0, z1
**	sqsubr	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_s8_m_untied, svint8_t,
		z0 = svqsubr_s8_m (p0, z1, z2),
		z0 = svqsubr_m (p0, z1, z2))

/*
** qsubr_w0_s8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	sqsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_s8_m_tied1, svint8_t, int8_t,
		 z0 = svqsubr_n_s8_m (p0, z0, x0),
		 z0 = svqsubr_m (p0, z0, x0))

/*
** qsubr_w0_s8_m_untied:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	sqsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_s8_m_untied, svint8_t, int8_t,
		 z0 = svqsubr_n_s8_m (p0, z1, x0),
		 z0 = svqsubr_m (p0, z1, x0))

/*
** qsubr_1_s8_m_tied1:
**	mov	(z[0-9]+\.b), #1
**	sqsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_s8_m_tied1, svint8_t,
		z0 = svqsubr_n_s8_m (p0, z0, 1),
		z0 = svqsubr_m (p0, z0, 1))

/*
** qsubr_1_s8_m_untied:
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0, z1
**	sqsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_s8_m_untied, svint8_t,
		z0 = svqsubr_n_s8_m (p0, z1, 1),
		z0 = svqsubr_m (p0, z1, 1))

/*
** qsubr_m1_s8_m:
**	mov	(z[0-9]+\.b), #-1
**	sqsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_m1_s8_m, svint8_t,
		z0 = svqsubr_n_s8_m (p0, z0, -1),
		z0 = svqsubr_m (p0, z0, -1))

/*
** qsubr_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	sqsubr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_s8_z_tied1, svint8_t,
		z0 = svqsubr_s8_z (p0, z0, z1),
		z0 = svqsubr_z (p0, z0, z1))

/*
** qsubr_s8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	sqsub	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_s8_z_tied2, svint8_t,
		z0 = svqsubr_s8_z (p0, z1, z0),
		z0 = svqsubr_z (p0, z1, z0))

/*
** qsubr_s8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	sqsubr	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	sqsub	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (qsubr_s8_z_untied, svint8_t,
		z0 = svqsubr_s8_z (p0, z1, z2),
		z0 = svqsubr_z (p0, z1, z2))

/*
** qsubr_w0_s8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	sqsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_s8_z_tied1, svint8_t, int8_t,
		 z0 = svqsubr_n_s8_z (p0, z0, x0),
		 z0 = svqsubr_z (p0, z0, x0))

/*
** qsubr_w0_s8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	sqsubr	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	sqsub	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_s8_z_untied, svint8_t, int8_t,
		 z0 = svqsubr_n_s8_z (p0, z1, x0),
		 z0 = svqsubr_z (p0, z1, x0))

/*
** qsubr_1_s8_z_tied1:
**	mov	(z[0-9]+\.b), #1
**	movprfx	z0\.b, p0/z, z0\.b
**	sqsubr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_s8_z_tied1, svint8_t,
		z0 = svqsubr_n_s8_z (p0, z0, 1),
		z0 = svqsubr_z (p0, z0, 1))

/*
** qsubr_1_s8_z_untied:
**	mov	(z[0-9]+\.b), #1
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	sqsubr	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	sqsub	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_s8_z_untied, svint8_t,
		z0 = svqsubr_n_s8_z (p0, z1, 1),
		z0 = svqsubr_z (p0, z1, 1))

/*
** qsubr_s8_x_tied1:
**	sqsub	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_s8_x_tied1, svint8_t,
		z0 = svqsubr_s8_x (p0, z0, z1),
		z0 = svqsubr_x (p0, z0, z1))

/*
** qsubr_s8_x_tied2:
**	sqsub	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_s8_x_tied2, svint8_t,
		z0 = svqsubr_s8_x (p0, z1, z0),
		z0 = svqsubr_x (p0, z1, z0))

/*
** qsubr_s8_x_untied:
**	sqsub	z0\.b, z2\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_s8_x_untied, svint8_t,
		z0 = svqsubr_s8_x (p0, z1, z2),
		z0 = svqsubr_x (p0, z1, z2))

/*
** qsubr_w0_s8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	sqsub	z0\.b, \1, z0\.b
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_s8_x_tied1, svint8_t, int8_t,
		 z0 = svqsubr_n_s8_x (p0, z0, x0),
		 z0 = svqsubr_x (p0, z0, x0))

/*
** qsubr_w0_s8_x_untied:
**	mov	(z[0-9]+\.b), w0
**	sqsub	z0\.b, \1, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_s8_x_untied, svint8_t, int8_t,
		 z0 = svqsubr_n_s8_x (p0, z1, x0),
		 z0 = svqsubr_x (p0, z1, x0))

/*
** qsubr_1_s8_x_tied1:
**	mov	(z[0-9]+\.b), #1
**	sqsub	z0\.b, \1, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_s8_x_tied1, svint8_t,
		z0 = svqsubr_n_s8_x (p0, z0, 1),
		z0 = svqsubr_x (p0, z0, 1))

/*
** qsubr_1_s8_x_untied:
**	mov	(z[0-9]+\.b), #1
**	sqsub	z0\.b, \1, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_s8_x_untied, svint8_t,
		z0 = svqsubr_n_s8_x (p0, z1, 1),
		z0 = svqsubr_x (p0, z1, 1))

/*
** qsubr_127_s8_x:
**	mov	(z[0-9]+\.b), #127
**	sqsub	z0\.b, \1, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_127_s8_x, svint8_t,
		z0 = svqsubr_n_s8_x (p0, z0, 127),
		z0 = svqsubr_x (p0, z0, 127))

/*
** qsubr_128_s8_x:
**	mov	(z[0-9]+\.b), #-128
**	sqsub	z0\.b, \1, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_128_s8_x, svint8_t,
		z0 = svqsubr_n_s8_x (p0, z0, 128),
		z0 = svqsubr_x (p0, z0, 128))

/*
** qsubr_255_s8_x:
**	mov	(z[0-9]+\.b), #-1
**	sqsub	z0\.b, \1, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_255_s8_x, svint8_t,
		z0 = svqsubr_n_s8_x (p0, z0, 255),
		z0 = svqsubr_x (p0, z0, 255))

/*
** qsubr_m1_s8_x:
**	mov	(z[0-9]+\.b), #-1
**	sqsub	z0\.b, \1, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_m1_s8_x, svint8_t,
		z0 = svqsubr_n_s8_x (p0, z0, -1),
		z0 = svqsubr_x (p0, z0, -1))

/*
** qsubr_m127_s8_x:
**	mov	(z[0-9]+\.b), #-127
**	sqsub	z0\.b, \1, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_m127_s8_x, svint8_t,
		z0 = svqsubr_n_s8_x (p0, z0, -127),
		z0 = svqsubr_x (p0, z0, -127))

/*
** qsubr_m128_s8_x:
**	mov	(z[0-9]+\.b), #-128
**	sqsub	z0\.b, \1, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qsubr_m128_s8_x, svint8_t,
		z0 = svqsubr_n_s8_x (p0, z0, -128),
		z0 = svqsubr_x (p0, z0, -128))
