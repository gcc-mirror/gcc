/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsubr_u32_m_tied1:
**	uqsubr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_u32_m_tied1, svuint32_t,
		z0 = svqsubr_u32_m (p0, z0, z1),
		z0 = svqsubr_m (p0, z0, z1))

/*
** qsubr_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uqsubr	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_u32_m_tied2, svuint32_t,
		z0 = svqsubr_u32_m (p0, z1, z0),
		z0 = svqsubr_m (p0, z1, z0))

/*
** qsubr_u32_m_untied:
**	movprfx	z0, z1
**	uqsubr	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_u32_m_untied, svuint32_t,
		z0 = svqsubr_u32_m (p0, z1, z2),
		z0 = svqsubr_m (p0, z1, z2))

/*
** qsubr_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	uqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_u32_m_tied1, svuint32_t, uint32_t,
		 z0 = svqsubr_n_u32_m (p0, z0, x0),
		 z0 = svqsubr_m (p0, z0, x0))

/*
** qsubr_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	uqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_u32_m_untied, svuint32_t, uint32_t,
		 z0 = svqsubr_n_u32_m (p0, z1, x0),
		 z0 = svqsubr_m (p0, z1, x0))

/*
** qsubr_1_u32_m_tied1:
**	mov	(z[0-9]+\.s), #1
**	uqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_u32_m_tied1, svuint32_t,
		z0 = svqsubr_n_u32_m (p0, z0, 1),
		z0 = svqsubr_m (p0, z0, 1))

/*
** qsubr_1_u32_m_untied:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0, z1
**	uqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_u32_m_untied, svuint32_t,
		z0 = svqsubr_n_u32_m (p0, z1, 1),
		z0 = svqsubr_m (p0, z1, 1))

/*
** qsubr_m2_u32_m:
**	mov	(z[0-9]+\.s), #-2
**	uqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_m2_u32_m, svuint32_t,
		z0 = svqsubr_n_u32_m (p0, z0, -2),
		z0 = svqsubr_m (p0, z0, -2))

/*
** qsubr_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsubr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_u32_z_tied1, svuint32_t,
		z0 = svqsubr_u32_z (p0, z0, z1),
		z0 = svqsubr_z (p0, z0, z1))

/*
** qsubr_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsub	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_u32_z_tied2, svuint32_t,
		z0 = svqsubr_u32_z (p0, z1, z0),
		z0 = svqsubr_z (p0, z1, z0))

/*
** qsubr_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqsubr	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	uqsub	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (qsubr_u32_z_untied, svuint32_t,
		z0 = svqsubr_u32_z (p0, z1, z2),
		z0 = svqsubr_z (p0, z1, z2))

/*
** qsubr_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_u32_z_tied1, svuint32_t, uint32_t,
		 z0 = svqsubr_n_u32_z (p0, z0, x0),
		 z0 = svqsubr_z (p0, z0, x0))

/*
** qsubr_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqsubr	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	uqsub	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_u32_z_untied, svuint32_t, uint32_t,
		 z0 = svqsubr_n_u32_z (p0, z1, x0),
		 z0 = svqsubr_z (p0, z1, x0))

/*
** qsubr_1_u32_z_tied1:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsubr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_u32_z_tied1, svuint32_t,
		z0 = svqsubr_n_u32_z (p0, z0, 1),
		z0 = svqsubr_z (p0, z0, 1))

/*
** qsubr_1_u32_z_untied:
**	mov	(z[0-9]+\.s), #1
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqsubr	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	uqsub	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_u32_z_untied, svuint32_t,
		z0 = svqsubr_n_u32_z (p0, z1, 1),
		z0 = svqsubr_z (p0, z1, 1))

/*
** qsubr_u32_x_tied1:
**	uqsub	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_u32_x_tied1, svuint32_t,
		z0 = svqsubr_u32_x (p0, z0, z1),
		z0 = svqsubr_x (p0, z0, z1))

/*
** qsubr_u32_x_tied2:
**	uqsub	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_u32_x_tied2, svuint32_t,
		z0 = svqsubr_u32_x (p0, z1, z0),
		z0 = svqsubr_x (p0, z1, z0))

/*
** qsubr_u32_x_untied:
**	uqsub	z0\.s, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_u32_x_untied, svuint32_t,
		z0 = svqsubr_u32_x (p0, z1, z2),
		z0 = svqsubr_x (p0, z1, z2))

/*
** qsubr_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	uqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_u32_x_tied1, svuint32_t, uint32_t,
		 z0 = svqsubr_n_u32_x (p0, z0, x0),
		 z0 = svqsubr_x (p0, z0, x0))

/*
** qsubr_w0_u32_x_untied:
**	mov	(z[0-9]+\.s), w0
**	uqsub	z0\.s, \1, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (qsubr_w0_u32_x_untied, svuint32_t, uint32_t,
		 z0 = svqsubr_n_u32_x (p0, z1, x0),
		 z0 = svqsubr_x (p0, z1, x0))

/*
** qsubr_1_u32_x_tied1:
**	mov	(z[0-9]+\.s), #1
**	uqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_u32_x_tied1, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z0, 1),
		z0 = svqsubr_x (p0, z0, 1))

/*
** qsubr_1_u32_x_untied:
**	mov	(z[0-9]+\.s), #1
**	uqsub	z0\.s, \1, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_u32_x_untied, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z1, 1),
		z0 = svqsubr_x (p0, z1, 1))

/*
** qsubr_127_u32_x:
**	mov	(z[0-9]+\.s), #127
**	uqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_127_u32_x, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z0, 127),
		z0 = svqsubr_x (p0, z0, 127))

/*
** qsubr_128_u32_x:
**	mov	(z[0-9]+\.s), #128
**	uqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_128_u32_x, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z0, 128),
		z0 = svqsubr_x (p0, z0, 128))

/*
** qsubr_255_u32_x:
**	mov	(z[0-9]+\.s), #255
**	uqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_255_u32_x, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z0, 255),
		z0 = svqsubr_x (p0, z0, 255))

/*
** qsubr_256_u32_x:
**	mov	(z[0-9]+\.s), #256
**	uqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_256_u32_x, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z0, 256),
		z0 = svqsubr_x (p0, z0, 256))

/*
** qsubr_511_u32_x:
**	mov	(z[0-9]+\.s), #511
**	uqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_511_u32_x, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z0, 511),
		z0 = svqsubr_x (p0, z0, 511))

/*
** qsubr_512_u32_x:
**	mov	(z[0-9]+\.s), #512
**	uqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_512_u32_x, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z0, 512),
		z0 = svqsubr_x (p0, z0, 512))

/*
** qsubr_65280_u32_x:
**	mov	(z[0-9]+\.s), #65280
**	uqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_65280_u32_x, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z0, 0xff00),
		z0 = svqsubr_x (p0, z0, 0xff00))

/*
** qsubr_65535_u32_x:
**	mov	(z[0-9]+\.s), #65535
**	uqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_65535_u32_x, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z0, 65535),
		z0 = svqsubr_x (p0, z0, 65535))

/*
** qsubr_65536_u32_x:
**	mov	(z[0-9]+\.s), #65536
**	uqsub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_65536_u32_x, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z0, 65536),
		z0 = svqsubr_x (p0, z0, 65536))

/*
** qsubr_m1_u32_x_tied1:
**	mov	(z[0-9]+)\.b, #-1
**	uqsub	z0\.s, \1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_m1_u32_x_tied1, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z0, -1),
		z0 = svqsubr_x (p0, z0, -1))

/*
** qsubr_m1_u32_x_untied:
**	mov	(z[0-9]+)\.b, #-1
**	uqsub	z0\.s, \1\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsubr_m1_u32_x_untied, svuint32_t,
		z0 = svqsubr_n_u32_x (p0, z1, -1),
		z0 = svqsubr_x (p0, z1, -1))
