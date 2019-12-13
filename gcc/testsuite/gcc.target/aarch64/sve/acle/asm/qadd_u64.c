/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qadd_u64_tied1:
**	uqadd	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (qadd_u64_tied1, svuint64_t,
		z0 = svqadd_u64 (z0, z1),
		z0 = svqadd (z0, z1))

/*
** qadd_u64_tied2:
**	uqadd	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (qadd_u64_tied2, svuint64_t,
		z0 = svqadd_u64 (z1, z0),
		z0 = svqadd (z1, z0))

/*
** qadd_u64_untied:
**	uqadd	z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (qadd_u64_untied, svuint64_t,
		z0 = svqadd_u64 (z1, z2),
		z0 = svqadd (z1, z2))

/*
** qadd_x0_u64_tied1:
**	mov	(z[0-9]+\.d), x0
**	uqadd	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_ZX (qadd_x0_u64_tied1, svuint64_t, uint64_t,
		 z0 = svqadd_n_u64 (z0, x0),
		 z0 = svqadd (z0, x0))

/*
** qadd_x0_u64_untied:
**	mov	(z[0-9]+\.d), x0
**	uqadd	z0\.d, (z1\.d, \1|\1, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (qadd_x0_u64_untied, svuint64_t, uint64_t,
		 z0 = svqadd_n_u64 (z1, x0),
		 z0 = svqadd (z1, x0))

/*
** qadd_1_u64_tied1:
**	uqadd	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_u64_tied1, svuint64_t,
		z0 = svqadd_n_u64 (z0, 1),
		z0 = svqadd (z0, 1))

/*
** qadd_1_u64_untied:
**	movprfx	z0, z1
**	uqadd	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_u64_untied, svuint64_t,
		z0 = svqadd_n_u64 (z1, 1),
		z0 = svqadd (z1, 1))

/*
** qadd_127_u64:
**	uqadd	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (qadd_127_u64, svuint64_t,
		z0 = svqadd_n_u64 (z0, 127),
		z0 = svqadd (z0, 127))

/*
** qadd_128_u64:
**	uqadd	z0\.d, z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (qadd_128_u64, svuint64_t,
		z0 = svqadd_n_u64 (z0, 128),
		z0 = svqadd (z0, 128))

/*
** qadd_255_u64:
**	uqadd	z0\.d, z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (qadd_255_u64, svuint64_t,
		z0 = svqadd_n_u64 (z0, 255),
		z0 = svqadd (z0, 255))

/*
** qadd_m1_u64:
**	mov	(z[0-9]+)\.b, #-1
**	uqadd	z0\.d, (z0\.d, \1\.d|\1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (qadd_m1_u64, svuint64_t,
		z0 = svqadd_n_u64 (z0, -1),
		z0 = svqadd (z0, -1))

/*
** qadd_m127_u64:
**	mov	(z[0-9]+\.d), #-127
**	uqadd	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (qadd_m127_u64, svuint64_t,
		z0 = svqadd_n_u64 (z0, -127),
		z0 = svqadd (z0, -127))

/*
** qadd_m128_u64:
**	mov	(z[0-9]+\.d), #-128
**	uqadd	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (qadd_m128_u64, svuint64_t,
		z0 = svqadd_n_u64 (z0, -128),
		z0 = svqadd (z0, -128))
