/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qadd_s64_tied1:
**	sqadd	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (qadd_s64_tied1, svint64_t,
		z0 = svqadd_s64 (z0, z1),
		z0 = svqadd (z0, z1))

/*
** qadd_s64_tied2:
**	sqadd	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (qadd_s64_tied2, svint64_t,
		z0 = svqadd_s64 (z1, z0),
		z0 = svqadd (z1, z0))

/*
** qadd_s64_untied:
**	sqadd	z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (qadd_s64_untied, svint64_t,
		z0 = svqadd_s64 (z1, z2),
		z0 = svqadd (z1, z2))

/*
** qadd_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	sqadd	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_ZX (qadd_x0_s64_tied1, svint64_t, int64_t,
		 z0 = svqadd_n_s64 (z0, x0),
		 z0 = svqadd (z0, x0))

/*
** qadd_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
**	sqadd	z0\.d, (z1\.d, \1|\1, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (qadd_x0_s64_untied, svint64_t, int64_t,
		 z0 = svqadd_n_s64 (z1, x0),
		 z0 = svqadd (z1, x0))

/*
** qadd_1_s64_tied1:
**	sqadd	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_s64_tied1, svint64_t,
		z0 = svqadd_n_s64 (z0, 1),
		z0 = svqadd (z0, 1))

/*
** qadd_1_s64_untied:
**	movprfx	z0, z1
**	sqadd	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_s64_untied, svint64_t,
		z0 = svqadd_n_s64 (z1, 1),
		z0 = svqadd (z1, 1))

/*
** qadd_127_s64:
**	sqadd	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (qadd_127_s64, svint64_t,
		z0 = svqadd_n_s64 (z0, 127),
		z0 = svqadd (z0, 127))

/*
** qadd_128_s64:
**	sqadd	z0\.d, z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (qadd_128_s64, svint64_t,
		z0 = svqadd_n_s64 (z0, 128),
		z0 = svqadd (z0, 128))

/*
** qadd_255_s64:
**	sqadd	z0\.d, z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (qadd_255_s64, svint64_t,
		z0 = svqadd_n_s64 (z0, 255),
		z0 = svqadd (z0, 255))

/*
** qadd_m1_s64:
**	sqsub	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_m1_s64, svint64_t,
		z0 = svqadd_n_s64 (z0, -1),
		z0 = svqadd (z0, -1))

/*
** qadd_m127_s64:
**	sqsub	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (qadd_m127_s64, svint64_t,
		z0 = svqadd_n_s64 (z0, -127),
		z0 = svqadd (z0, -127))

/*
** qadd_m128_s64:
**	sqsub	z0\.d, z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (qadd_m128_s64, svint64_t,
		z0 = svqadd_n_s64 (z0, -128),
		z0 = svqadd (z0, -128))
