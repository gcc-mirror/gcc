/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsub_s64_tied1:
**	sqsub	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_s64_tied1, svint64_t,
		z0 = svqsub_s64 (z0, z1),
		z0 = svqsub (z0, z1))

/*
** qsub_s64_tied2:
**	sqsub	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_s64_tied2, svint64_t,
		z0 = svqsub_s64 (z1, z0),
		z0 = svqsub (z1, z0))

/*
** qsub_s64_untied:
**	sqsub	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_s64_untied, svint64_t,
		z0 = svqsub_s64 (z1, z2),
		z0 = svqsub (z1, z2))

/*
** qsub_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	sqsub	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_x0_s64_tied1, svint64_t, int64_t,
		 z0 = svqsub_n_s64 (z0, x0),
		 z0 = svqsub (z0, x0))

/*
** qsub_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
**	sqsub	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_x0_s64_untied, svint64_t, int64_t,
		 z0 = svqsub_n_s64 (z1, x0),
		 z0 = svqsub (z1, x0))

/*
** qsub_1_s64_tied1:
**	sqsub	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s64_tied1, svint64_t,
		z0 = svqsub_n_s64 (z0, 1),
		z0 = svqsub (z0, 1))

/*
** qsub_1_s64_untied:
**	movprfx	z0, z1
**	sqsub	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s64_untied, svint64_t,
		z0 = svqsub_n_s64 (z1, 1),
		z0 = svqsub (z1, 1))

/*
** qsub_127_s64:
**	sqsub	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_s64, svint64_t,
		z0 = svqsub_n_s64 (z0, 127),
		z0 = svqsub (z0, 127))

/*
** qsub_128_s64:
**	sqsub	z0\.d, z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_s64, svint64_t,
		z0 = svqsub_n_s64 (z0, 128),
		z0 = svqsub (z0, 128))

/*
** qsub_255_s64:
**	sqsub	z0\.d, z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_s64, svint64_t,
		z0 = svqsub_n_s64 (z0, 255),
		z0 = svqsub (z0, 255))

/*
** qsub_m1_s64:
**	sqadd	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_s64, svint64_t,
		z0 = svqsub_n_s64 (z0, -1),
		z0 = svqsub (z0, -1))

/*
** qsub_m127_s64:
**	sqadd	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_s64, svint64_t,
		z0 = svqsub_n_s64 (z0, -127),
		z0 = svqsub (z0, -127))

/*
** qsub_m128_s64:
**	sqadd	z0\.d, z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_s64, svint64_t,
		z0 = svqsub_n_s64 (z0, -128),
		z0 = svqsub (z0, -128))

/*
** qsub_s64_m_tied1:
**	sqsub	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_s64_m_tied1, svint64_t,
		z0 = svqsub_s64_m (p0, z0, z1),
		z0 = svqsub_m (p0, z0, z1))

/*
** qsub_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_s64_m_tied2, svint64_t,
		z0 = svqsub_s64_m (p0, z1, z0),
		z0 = svqsub_m (p0, z1, z0))

/*
** qsub_s64_m_untied:
**	movprfx	z0, z1
**	sqsub	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_s64_m_untied, svint64_t,
		z0 = svqsub_s64_m (p0, z1, z2),
		z0 = svqsub_m (p0, z1, z2))

/*
** qsub_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_x0_s64_m_tied1, svint64_t, int64_t,
		 z0 = svqsub_n_s64_m (p0, z0, x0),
		 z0 = svqsub_m (p0, z0, x0))

/*
** qsub_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_x0_s64_m_untied, svint64_t, int64_t,
		 z0 = svqsub_n_s64_m (p0, z1, x0),
		 z0 = svqsub_m (p0, z1, x0))

/*
** qsub_1_s64_m_tied1:
**	mov	(z[0-9]+\.d), #1
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s64_m_tied1, svint64_t,
		z0 = svqsub_n_s64_m (p0, z0, 1),
		z0 = svqsub_m (p0, z0, 1))

/*
** qsub_1_s64_m_untied:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0, z1
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s64_m_untied, svint64_t,
		z0 = svqsub_n_s64_m (p0, z1, 1),
		z0 = svqsub_m (p0, z1, 1))

/*
** qsub_127_s64_m:
**	mov	(z[0-9]+\.d), #127
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_127_s64_m, svint64_t,
		z0 = svqsub_n_s64_m (p0, z0, 127),
		z0 = svqsub_m (p0, z0, 127))

/*
** qsub_128_s64_m:
**	mov	(z[0-9]+\.d), #128
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_128_s64_m, svint64_t,
		z0 = svqsub_n_s64_m (p0, z0, 128),
		z0 = svqsub_m (p0, z0, 128))

/*
** qsub_255_s64_m:
**	mov	(z[0-9]+\.d), #255
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_255_s64_m, svint64_t,
		z0 = svqsub_n_s64_m (p0, z0, 255),
		z0 = svqsub_m (p0, z0, 255))

/*
** qsub_m1_s64_m:
**	mov	(z[0-9]+)\.b, #-1
**	sqsub	z0\.d, p0/m, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_s64_m, svint64_t,
		z0 = svqsub_n_s64_m (p0, z0, -1),
		z0 = svqsub_m (p0, z0, -1))

/*
** qsub_m127_s64_m:
**	mov	(z[0-9]+\.d), #-127
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_s64_m, svint64_t,
		z0 = svqsub_n_s64_m (p0, z0, -127),
		z0 = svqsub_m (p0, z0, -127))

/*
** qsub_m128_s64_m:
**	mov	(z[0-9]+\.d), #-128
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_s64_m, svint64_t,
		z0 = svqsub_n_s64_m (p0, z0, -128),
		z0 = svqsub_m (p0, z0, -128))

/*
** qsub_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqsub	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_s64_z_tied1, svint64_t,
		z0 = svqsub_s64_z (p0, z0, z1),
		z0 = svqsub_z (p0, z0, z1))

/*
** qsub_s64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqsubr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_s64_z_tied2, svint64_t,
		z0 = svqsub_s64_z (p0, z1, z0),
		z0 = svqsub_z (p0, z1, z0))

/*
** qsub_s64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	sqsub	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	sqsubr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (qsub_s64_z_untied, svint64_t,
		z0 = svqsub_s64_z (p0, z1, z2),
		z0 = svqsub_z (p0, z1, z2))

/*
** qsub_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_x0_s64_z_tied1, svint64_t, int64_t,
		 z0 = svqsub_n_s64_z (p0, z0, x0),
		 z0 = svqsub_z (p0, z0, x0))

/*
** qsub_x0_s64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	sqsub	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	sqsubr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (qsub_x0_s64_z_untied, svint64_t, int64_t,
		 z0 = svqsub_n_s64_z (p0, z1, x0),
		 z0 = svqsub_z (p0, z1, x0))

/*
** qsub_1_s64_z_tied1:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0\.d, p0/z, z0\.d
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s64_z_tied1, svint64_t,
		z0 = svqsub_n_s64_z (p0, z0, 1),
		z0 = svqsub_z (p0, z0, 1))

/*
** qsub_1_s64_z_untied:
**	mov	(z[0-9]+\.d), #1
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	sqsub	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	sqsubr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s64_z_untied, svint64_t,
		z0 = svqsub_n_s64_z (p0, z1, 1),
		z0 = svqsub_z (p0, z1, 1))

/*
** qsub_127_s64_z:
**	mov	(z[0-9]+\.d), #127
**	movprfx	z0\.d, p0/z, z0\.d
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_127_s64_z, svint64_t,
		z0 = svqsub_n_s64_z (p0, z0, 127),
		z0 = svqsub_z (p0, z0, 127))

/*
** qsub_128_s64_z:
**	mov	(z[0-9]+\.d), #128
**	movprfx	z0\.d, p0/z, z0\.d
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_128_s64_z, svint64_t,
		z0 = svqsub_n_s64_z (p0, z0, 128),
		z0 = svqsub_z (p0, z0, 128))

/*
** qsub_255_s64_z:
**	mov	(z[0-9]+\.d), #255
**	movprfx	z0\.d, p0/z, z0\.d
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_255_s64_z, svint64_t,
		z0 = svqsub_n_s64_z (p0, z0, 255),
		z0 = svqsub_z (p0, z0, 255))

/*
** qsub_m1_s64_z:
**	mov	(z[0-9]+)\.b, #-1
**	movprfx	z0\.d, p0/z, z0\.d
**	sqsub	z0\.d, p0/m, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_s64_z, svint64_t,
		z0 = svqsub_n_s64_z (p0, z0, -1),
		z0 = svqsub_z (p0, z0, -1))

/*
** qsub_m127_s64_z:
**	mov	(z[0-9]+\.d), #-127
**	movprfx	z0\.d, p0/z, z0\.d
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_s64_z, svint64_t,
		z0 = svqsub_n_s64_z (p0, z0, -127),
		z0 = svqsub_z (p0, z0, -127))

/*
** qsub_m128_s64_z:
**	mov	(z[0-9]+\.d), #-128
**	movprfx	z0\.d, p0/z, z0\.d
**	sqsub	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_s64_z, svint64_t,
		z0 = svqsub_n_s64_z (p0, z0, -128),
		z0 = svqsub_z (p0, z0, -128))

/*
** qsub_s64_x_tied1:
**	sqsub	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_s64_x_tied1, svint64_t,
		z0 = svqsub_s64_x (p0, z0, z1),
		z0 = svqsub_x (p0, z0, z1))

/*
** qsub_s64_x_tied2:
**	sqsub	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_s64_x_tied2, svint64_t,
		z0 = svqsub_s64_x (p0, z1, z0),
		z0 = svqsub_x (p0, z1, z0))

/*
** qsub_s64_x_untied:
**	sqsub	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_s64_x_untied, svint64_t,
		z0 = svqsub_s64_x (p0, z1, z2),
		z0 = svqsub_x (p0, z1, z2))

/*
** qsub_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	sqsub	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_x0_s64_x_tied1, svint64_t, int64_t,
		 z0 = svqsub_n_s64_x (p0, z0, x0),
		 z0 = svqsub_x (p0, z0, x0))

/*
** qsub_x0_s64_x_untied:
**	mov	(z[0-9]+\.d), x0
**	sqsub	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_x0_s64_x_untied, svint64_t, int64_t,
		 z0 = svqsub_n_s64_x (p0, z1, x0),
		 z0 = svqsub_x (p0, z1, x0))

/*
** qsub_1_s64_x_tied1:
**	sqsub	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s64_x_tied1, svint64_t,
		z0 = svqsub_n_s64_x (p0, z0, 1),
		z0 = svqsub_x (p0, z0, 1))

/*
** qsub_1_s64_x_untied:
**	movprfx	z0, z1
**	sqsub	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s64_x_untied, svint64_t,
		z0 = svqsub_n_s64_x (p0, z1, 1),
		z0 = svqsub_x (p0, z1, 1))

/*
** qsub_127_s64_x:
**	sqsub	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_s64_x, svint64_t,
		z0 = svqsub_n_s64_x (p0, z0, 127),
		z0 = svqsub_x (p0, z0, 127))

/*
** qsub_128_s64_x:
**	sqsub	z0\.d, z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_s64_x, svint64_t,
		z0 = svqsub_n_s64_x (p0, z0, 128),
		z0 = svqsub_x (p0, z0, 128))

/*
** qsub_255_s64_x:
**	sqsub	z0\.d, z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_s64_x, svint64_t,
		z0 = svqsub_n_s64_x (p0, z0, 255),
		z0 = svqsub_x (p0, z0, 255))

/*
** qsub_m1_s64_x:
**	sqadd	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_s64_x, svint64_t,
		z0 = svqsub_n_s64_x (p0, z0, -1),
		z0 = svqsub_x (p0, z0, -1))

/*
** qsub_m127_s64_x:
**	sqadd	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_s64_x, svint64_t,
		z0 = svqsub_n_s64_x (p0, z0, -127),
		z0 = svqsub_x (p0, z0, -127))

/*
** qsub_m128_s64_x:
**	sqadd	z0\.d, z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_s64_x, svint64_t,
		z0 = svqsub_n_s64_x (p0, z0, -128),
		z0 = svqsub_x (p0, z0, -128))
