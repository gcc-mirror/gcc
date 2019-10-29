/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsub_u64_tied1:
**	uqsub	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_u64_tied1, svuint64_t,
		z0 = svqsub_u64 (z0, z1),
		z0 = svqsub (z0, z1))

/*
** qsub_u64_tied2:
**	uqsub	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_u64_tied2, svuint64_t,
		z0 = svqsub_u64 (z1, z0),
		z0 = svqsub (z1, z0))

/*
** qsub_u64_untied:
**	uqsub	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_u64_untied, svuint64_t,
		z0 = svqsub_u64 (z1, z2),
		z0 = svqsub (z1, z2))

/*
** qsub_x0_u64_tied1:
**	mov	(z[0-9]+\.d), x0
**	uqsub	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_x0_u64_tied1, svuint64_t, uint64_t,
		 z0 = svqsub_n_u64 (z0, x0),
		 z0 = svqsub (z0, x0))

/*
** qsub_x0_u64_untied:
**	mov	(z[0-9]+\.d), x0
**	uqsub	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_x0_u64_untied, svuint64_t, uint64_t,
		 z0 = svqsub_n_u64 (z1, x0),
		 z0 = svqsub (z1, x0))

/*
** qsub_1_u64_tied1:
**	uqsub	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u64_tied1, svuint64_t,
		z0 = svqsub_n_u64 (z0, 1),
		z0 = svqsub (z0, 1))

/*
** qsub_1_u64_untied:
**	movprfx	z0, z1
**	uqsub	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u64_untied, svuint64_t,
		z0 = svqsub_n_u64 (z1, 1),
		z0 = svqsub (z1, 1))

/*
** qsub_127_u64:
**	uqsub	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u64, svuint64_t,
		z0 = svqsub_n_u64 (z0, 127),
		z0 = svqsub (z0, 127))

/*
** qsub_128_u64:
**	uqsub	z0\.d, z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u64, svuint64_t,
		z0 = svqsub_n_u64 (z0, 128),
		z0 = svqsub (z0, 128))

/*
** qsub_255_u64:
**	uqsub	z0\.d, z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u64, svuint64_t,
		z0 = svqsub_n_u64 (z0, 255),
		z0 = svqsub (z0, 255))

/*
** qsub_m1_u64:
**	mov	(z[0-9]+)\.b, #-1
**	uqsub	z0\.d, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u64, svuint64_t,
		z0 = svqsub_n_u64 (z0, -1),
		z0 = svqsub (z0, -1))

/*
** qsub_m127_u64:
**	mov	(z[0-9]+\.d), #-127
**	uqsub	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u64, svuint64_t,
		z0 = svqsub_n_u64 (z0, -127),
		z0 = svqsub (z0, -127))

/*
** qsub_m128_u64:
**	mov	(z[0-9]+\.d), #-128
**	uqsub	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u64, svuint64_t,
		z0 = svqsub_n_u64 (z0, -128),
		z0 = svqsub (z0, -128))
