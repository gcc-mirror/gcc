/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsubr_u64_m_tied1:
**	uqsubr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_u64_m_tied1, svuint64_t,
		z0 = svqsubr_u64_m (p0, z0, z1),
		z0 = svqsubr_m (p0, z0, z1))

/*
** qsubr_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	uqsubr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_u64_m_tied2, svuint64_t,
		z0 = svqsubr_u64_m (p0, z1, z0),
		z0 = svqsubr_m (p0, z1, z0))

/*
** qsubr_u64_m_untied:
**	movprfx	z0, z1
**	uqsubr	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_u64_m_untied, svuint64_t,
		z0 = svqsubr_u64_m (p0, z1, z2),
		z0 = svqsubr_m (p0, z1, z2))

/*
** qsubr_x0_u64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	uqsubr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qsubr_x0_u64_m_tied1, svuint64_t, uint64_t,
		 z0 = svqsubr_n_u64_m (p0, z0, x0),
		 z0 = svqsubr_m (p0, z0, x0))

/*
** qsubr_x0_u64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	uqsubr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qsubr_x0_u64_m_untied, svuint64_t, uint64_t,
		 z0 = svqsubr_n_u64_m (p0, z1, x0),
		 z0 = svqsubr_m (p0, z1, x0))

/*
** qsubr_1_u64_m_tied1:
**	mov	(z[0-9]+\.d), #1
**	uqsubr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_u64_m_tied1, svuint64_t,
		z0 = svqsubr_n_u64_m (p0, z0, 1),
		z0 = svqsubr_m (p0, z0, 1))

/*
** qsubr_1_u64_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0, z1
**	uqsubr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_u64_m_untied, svuint64_t,
		z0 = svqsubr_n_u64_m (p0, z1, 1),
		z0 = svqsubr_m (p0, z1, 1))

/*
** qsubr_m2_u64_m:
**	mov	(z[0-9]+\.d), #-2
**	uqsubr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_m2_u64_m, svuint64_t,
		z0 = svqsubr_n_u64_m (p0, z0, -2),
		z0 = svqsubr_m (p0, z0, -2))

/*
** qsubr_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	uqsubr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_u64_z_tied1, svuint64_t,
		z0 = svqsubr_u64_z (p0, z0, z1),
		z0 = svqsubr_z (p0, z0, z1))

/*
** qsubr_u64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	uqsub	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_u64_z_tied2, svuint64_t,
		z0 = svqsubr_u64_z (p0, z1, z0),
		z0 = svqsubr_z (p0, z1, z0))

/*
** qsubr_u64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	uqsubr	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	uqsub	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (qsubr_u64_z_untied, svuint64_t,
		z0 = svqsubr_u64_z (p0, z1, z2),
		z0 = svqsubr_z (p0, z1, z2))

/*
** qsubr_x0_u64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	uqsubr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qsubr_x0_u64_z_tied1, svuint64_t, uint64_t,
		 z0 = svqsubr_n_u64_z (p0, z0, x0),
		 z0 = svqsubr_z (p0, z0, x0))

/*
** qsubr_x0_u64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	uqsubr	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	uqsub	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (qsubr_x0_u64_z_untied, svuint64_t, uint64_t,
		 z0 = svqsubr_n_u64_z (p0, z1, x0),
		 z0 = svqsubr_z (p0, z1, x0))

/*
** qsubr_1_u64_z_tied1:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0\.d, p0/z, z0\.d
**	uqsubr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_u64_z_tied1, svuint64_t,
		z0 = svqsubr_n_u64_z (p0, z0, 1),
		z0 = svqsubr_z (p0, z0, 1))

/*
** qsubr_1_u64_z_untied:
**	mov	(z[0-9]+\.d), #1
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	uqsubr	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	uqsub	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_u64_z_untied, svuint64_t,
		z0 = svqsubr_n_u64_z (p0, z1, 1),
		z0 = svqsubr_z (p0, z1, 1))

/*
** qsubr_u64_x_tied1:
**	uqsub	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_u64_x_tied1, svuint64_t,
		z0 = svqsubr_u64_x (p0, z0, z1),
		z0 = svqsubr_x (p0, z0, z1))

/*
** qsubr_u64_x_tied2:
**	uqsub	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_u64_x_tied2, svuint64_t,
		z0 = svqsubr_u64_x (p0, z1, z0),
		z0 = svqsubr_x (p0, z1, z0))

/*
** qsubr_u64_x_untied:
**	uqsub	z0\.d, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_u64_x_untied, svuint64_t,
		z0 = svqsubr_u64_x (p0, z1, z2),
		z0 = svqsubr_x (p0, z1, z2))

/*
** qsubr_x0_u64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	uqsub	z0\.d, \1, z0\.d
**	ret
*/
TEST_UNIFORM_ZX (qsubr_x0_u64_x_tied1, svuint64_t, uint64_t,
		 z0 = svqsubr_n_u64_x (p0, z0, x0),
		 z0 = svqsubr_x (p0, z0, x0))

/*
** qsubr_x0_u64_x_untied:
**	mov	(z[0-9]+\.d), x0
**	uqsub	z0\.d, \1, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (qsubr_x0_u64_x_untied, svuint64_t, uint64_t,
		 z0 = svqsubr_n_u64_x (p0, z1, x0),
		 z0 = svqsubr_x (p0, z1, x0))

/*
** qsubr_1_u64_x_tied1:
**	mov	(z[0-9]+\.d), #1
**	uqsub	z0\.d, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_u64_x_tied1, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z0, 1),
		z0 = svqsubr_x (p0, z0, 1))

/*
** qsubr_1_u64_x_untied:
**	mov	(z[0-9]+\.d), #1
**	uqsub	z0\.d, \1, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_1_u64_x_untied, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z1, 1),
		z0 = svqsubr_x (p0, z1, 1))

/*
** qsubr_127_u64_x:
**	mov	(z[0-9]+\.d), #127
**	uqsub	z0\.d, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_127_u64_x, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z0, 127),
		z0 = svqsubr_x (p0, z0, 127))

/*
** qsubr_128_u64_x:
**	mov	(z[0-9]+\.d), #128
**	uqsub	z0\.d, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_128_u64_x, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z0, 128),
		z0 = svqsubr_x (p0, z0, 128))

/*
** qsubr_255_u64_x:
**	mov	(z[0-9]+\.d), #255
**	uqsub	z0\.d, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_255_u64_x, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z0, 255),
		z0 = svqsubr_x (p0, z0, 255))

/*
** qsubr_256_u64_x:
**	mov	(z[0-9]+\.d), #256
**	uqsub	z0\.d, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_256_u64_x, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z0, 256),
		z0 = svqsubr_x (p0, z0, 256))

/*
** qsubr_511_u64_x:
**	mov	(z[0-9]+\.d), #511
**	uqsub	z0\.d, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_511_u64_x, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z0, 511),
		z0 = svqsubr_x (p0, z0, 511))

/*
** qsubr_512_u64_x:
**	mov	(z[0-9]+\.d), #512
**	uqsub	z0\.d, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_512_u64_x, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z0, 512),
		z0 = svqsubr_x (p0, z0, 512))

/*
** qsubr_65280_u64_x:
**	mov	(z[0-9]+\.d), #65280
**	uqsub	z0\.d, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_65280_u64_x, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z0, 0xff00),
		z0 = svqsubr_x (p0, z0, 0xff00))

/*
** qsubr_65535_u64_x:
**	mov	(z[0-9]+\.d), #65535
**	uqsub	z0\.d, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_65535_u64_x, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z0, 65535),
		z0 = svqsubr_x (p0, z0, 65535))

/*
** qsubr_65536_u64_x:
**	mov	(z[0-9]+\.d), #65536
**	uqsub	z0\.d, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_65536_u64_x, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z0, 65536),
		z0 = svqsubr_x (p0, z0, 65536))

/*
** qsubr_m1_u64_x_tied1:
**	mov	(z[0-9]+)\.b, #-1
**	uqsub	z0\.d, \1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_m1_u64_x_tied1, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z0, -1),
		z0 = svqsubr_x (p0, z0, -1))

/*
** qsubr_m1_u64_x_untied:
**	mov	(z[0-9]+)\.b, #-1
**	uqsub	z0\.d, \1\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsubr_m1_u64_x_untied, svuint64_t,
		z0 = svqsubr_n_u64_x (p0, z1, -1),
		z0 = svqsubr_x (p0, z1, -1))
