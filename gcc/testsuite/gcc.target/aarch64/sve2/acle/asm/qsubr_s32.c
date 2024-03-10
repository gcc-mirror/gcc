/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsubr_s32_m_tied1:
**	sqsubr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_s32_m_tied1, svint32_t,
		z0 = svqsubr_s32_m (p0, z0, z1),
		z0 = svqsubr_m (p0, z0, z1))

/*
** qsubr_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqsubr	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_s32_m_tied2, svint32_t,
		z0 = svqsubr_s32_m (p0, z1, z0),
		z0 = svqsubr_m (p0, z1, z0))

/*
** qsubr_s32_m_untied:
**	movprfx	z0, z1
**	sqsubr	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_s32_m_untied, svint32_t,
		z0 = svqsubr_s32_m (p0, z1, z2),
		z0 = svqsubr_m (p0, z1, z2))

/*
** qsubr_w0_s32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	sqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_s32_m_tied1, svint32_t, int32_t,
		 z0 = svqsubr_n_s32_m (p0, z0, x0),
		 z0 = svqsubr_m (p0, z0, x0))

/*
** qsubr_w0_s32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	sqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_s32_m_untied, svint32_t, int32_t,
		 z0 = svqsubr_n_s32_m (p0, z1, x0),
		 z0 = svqsubr_m (p0, z1, x0))

/*
** qsubr_1_s32_m_tied1:
**	mov	(z[0-9]+\.s), #1
**	sqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_s32_m_tied1, svint32_t,
		z0 = svqsubr_n_s32_m (p0, z0, 1),
		z0 = svqsubr_m (p0, z0, 1))

/*
** qsubr_1_s32_m_untied:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0, z1
**	sqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_s32_m_untied, svint32_t,
		z0 = svqsubr_n_s32_m (p0, z1, 1),
		z0 = svqsubr_m (p0, z1, 1))

/*
** qsubr_m2_s32_m:
**	mov	(z[0-9]+\.s), #-2
**	sqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_m2_s32_m, svint32_t,
		z0 = svqsubr_n_s32_m (p0, z0, -2),
		z0 = svqsubr_m (p0, z0, -2))

/*
** qsubr_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	sqsubr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_s32_z_tied1, svint32_t,
		z0 = svqsubr_s32_z (p0, z0, z1),
		z0 = svqsubr_z (p0, z0, z1))

/*
** qsubr_s32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	sqsub	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_s32_z_tied2, svint32_t,
		z0 = svqsubr_s32_z (p0, z1, z0),
		z0 = svqsubr_z (p0, z1, z0))

/*
** qsubr_s32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	sqsubr	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	sqsub	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (qsubr_s32_z_untied, svint32_t,
		z0 = svqsubr_s32_z (p0, z1, z2),
		z0 = svqsubr_z (p0, z1, z2))

/*
** qsubr_w0_s32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	sqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_s32_z_tied1, svint32_t, int32_t,
		 z0 = svqsubr_n_s32_z (p0, z0, x0),
		 z0 = svqsubr_z (p0, z0, x0))

/*
** qsubr_w0_s32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	sqsubr	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	sqsub	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_s32_z_untied, svint32_t, int32_t,
		 z0 = svqsubr_n_s32_z (p0, z1, x0),
		 z0 = svqsubr_z (p0, z1, x0))

/*
** qsubr_1_s32_z_tied1:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z0\.s
**	sqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_s32_z_tied1, svint32_t,
		z0 = svqsubr_n_s32_z (p0, z0, 1),
		z0 = svqsubr_z (p0, z0, 1))

/*
** qsubr_1_s32_z_untied:
**	mov	(z[0-9]+\.s), #1
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	sqsubr	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	sqsub	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_s32_z_untied, svint32_t,
		z0 = svqsubr_n_s32_z (p0, z1, 1),
		z0 = svqsubr_z (p0, z1, 1))

/*
** qsubr_s32_x_tied1:
**	sqsub	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_s32_x_tied1, svint32_t,
		z0 = svqsubr_s32_x (p0, z0, z1),
		z0 = svqsubr_x (p0, z0, z1))

/*
** qsubr_s32_x_tied2:
**	sqsub	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_s32_x_tied2, svint32_t,
		z0 = svqsubr_s32_x (p0, z1, z0),
		z0 = svqsubr_x (p0, z1, z0))

/*
** qsubr_s32_x_untied:
**	sqsub	z0\.s, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_s32_x_untied, svint32_t,
		z0 = svqsubr_s32_x (p0, z1, z2),
		z0 = svqsubr_x (p0, z1, z2))

/*
** qsubr_w0_s32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	sqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_s32_x_tied1, svint32_t, int32_t,
		 z0 = svqsubr_n_s32_x (p0, z0, x0),
		 z0 = svqsubr_x (p0, z0, x0))

/*
** qsubr_w0_s32_x_untied:
**	mov	(z[0-9]+\.s), w0
**	sqsub	z0\.s, \1, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_s32_x_untied, svint32_t, int32_t,
		 z0 = svqsubr_n_s32_x (p0, z1, x0),
		 z0 = svqsubr_x (p0, z1, x0))

/*
** qsubr_1_s32_x_tied1:
**	mov	(z[0-9]+\.s), #1
**	sqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_s32_x_tied1, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z0, 1),
		z0 = svqsubr_x (p0, z0, 1))

/*
** qsubr_1_s32_x_untied:
**	mov	(z[0-9]+\.s), #1
**	sqsub	z0\.s, \1, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_s32_x_untied, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z1, 1),
		z0 = svqsubr_x (p0, z1, 1))

/*
** qsubr_127_s32_x:
**	mov	(z[0-9]+\.s), #127
**	sqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_127_s32_x, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z0, 127),
		z0 = svqsubr_x (p0, z0, 127))

/*
** qsubr_128_s32_x:
**	mov	(z[0-9]+\.s), #128
**	sqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_128_s32_x, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z0, 128),
		z0 = svqsubr_x (p0, z0, 128))

/*
** qsubr_255_s32_x:
**	mov	(z[0-9]+\.s), #255
**	sqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_255_s32_x, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z0, 255),
		z0 = svqsubr_x (p0, z0, 255))

/*
** qsubr_256_s32_x:
**	mov	(z[0-9]+\.s), #256
**	sqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_256_s32_x, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z0, 256),
		z0 = svqsubr_x (p0, z0, 256))

/*
** qsubr_511_s32_x:
**	mov	(z[0-9]+\.s), #511
**	sqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_511_s32_x, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z0, 511),
		z0 = svqsubr_x (p0, z0, 511))

/*
** qsubr_512_s32_x:
**	mov	(z[0-9]+\.s), #512
**	sqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_512_s32_x, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z0, 512),
		z0 = svqsubr_x (p0, z0, 512))

/*
** qsubr_65280_s32_x:
**	mov	(z[0-9]+\.s), #65280
**	sqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_65280_s32_x, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z0, 0xff00),
		z0 = svqsubr_x (p0, z0, 0xff00))

/*
** qsubr_65535_s32_x:
**	mov	(z[0-9]+\.s), #65535
**	sqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_65535_s32_x, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z0, 65535),
		z0 = svqsubr_x (p0, z0, 65535))

/*
** qsubr_65536_s32_x:
**	mov	(z[0-9]+\.s), #65536
**	sqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_65536_s32_x, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z0, 65536),
		z0 = svqsubr_x (p0, z0, 65536))

/*
** qsubr_m1_s32_x_tied1:
**	mov	(z[0-9]+)\.b, #-1
**	sqsub	z0\.s, \1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_m1_s32_x_tied1, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z0, -1),
		z0 = svqsubr_x (p0, z0, -1))

/*
** qsubr_m1_s32_x_untied:
**	mov	(z[0-9]+)\.b, #-1
**	sqsub	z0\.s, \1\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_m1_s32_x_untied, svint32_t,
		z0 = svqsubr_n_s32_x (p0, z1, -1),
		z0 = svqsubr_x (p0, z1, -1))
