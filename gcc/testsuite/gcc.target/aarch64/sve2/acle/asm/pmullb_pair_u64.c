/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-aes"

/*
** pmullb_pair_u64_tied1:
**	pmullb	z0\.q, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (pmullb_pair_u64_tied1, svuint64_t,
		z0 = svpmullb_pair_u64 (z0, z1),
		z0 = svpmullb_pair (z0, z1))

/*
** pmullb_pair_u64_tied2:
**	pmullb	z0\.q, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (pmullb_pair_u64_tied2, svuint64_t,
		z0 = svpmullb_pair_u64 (z1, z0),
		z0 = svpmullb_pair (z1, z0))

/*
** pmullb_pair_u64_untied:
**	pmullb	z0\.q, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (pmullb_pair_u64_untied, svuint64_t,
		z0 = svpmullb_pair_u64 (z1, z2),
		z0 = svpmullb_pair (z1, z2))

/*
** pmullb_pair_x0_u64_tied1:
**	mov	(z[0-9]+\.d), x0
**	pmullb	z0\.q, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (pmullb_pair_x0_u64_tied1, svuint64_t, uint64_t,
		 z0 = svpmullb_pair_n_u64 (z0, x0),
		 z0 = svpmullb_pair (z0, x0))

/*
** pmullb_pair_x0_u64_untied:
**	mov	(z[0-9]+\.d), x0
**	pmullb	z0\.q, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (pmullb_pair_x0_u64_untied, svuint64_t, uint64_t,
		 z0 = svpmullb_pair_n_u64 (z1, x0),
		 z0 = svpmullb_pair (z1, x0))

/*
** pmullb_pair_11_u64_tied1:
**	mov	(z[0-9]+\.d), #11
**	pmullb	z0\.q, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (pmullb_pair_11_u64_tied1, svuint64_t,
		z0 = svpmullb_pair_n_u64 (z0, 11),
		z0 = svpmullb_pair (z0, 11))

/*
** pmullb_pair_11_u64_untied:
**	mov	(z[0-9]+\.d), #11
**	pmullb	z0\.q, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (pmullb_pair_11_u64_untied, svuint64_t,
		z0 = svpmullb_pair_n_u64 (z1, 11),
		z0 = svpmullb_pair (z1, 11))
