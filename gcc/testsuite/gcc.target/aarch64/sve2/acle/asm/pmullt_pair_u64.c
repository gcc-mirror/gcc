/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-aes"

/*
** pmullt_pair_u64_tied1:
**	pmullt	z0\.q, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_u64_tied1, svuint64_t,
		z0 = svpmullt_pair_u64 (z0, z1),
		z0 = svpmullt_pair (z0, z1))

/*
** pmullt_pair_u64_tied2:
**	pmullt	z0\.q, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_u64_tied2, svuint64_t,
		z0 = svpmullt_pair_u64 (z1, z0),
		z0 = svpmullt_pair (z1, z0))

/*
** pmullt_pair_u64_untied:
**	pmullt	z0\.q, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_u64_untied, svuint64_t,
		z0 = svpmullt_pair_u64 (z1, z2),
		z0 = svpmullt_pair (z1, z2))

/*
** pmullt_pair_x0_u64_tied1:
**	mov	(z[0-9]+\.d), x0
**	pmullt	z0\.q, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (pmullt_pair_x0_u64_tied1, svuint64_t, uint64_t,
		 z0 = svpmullt_pair_n_u64 (z0, x0),
		 z0 = svpmullt_pair (z0, x0))

/*
** pmullt_pair_x0_u64_untied:
**	mov	(z[0-9]+\.d), x0
**	pmullt	z0\.q, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (pmullt_pair_x0_u64_untied, svuint64_t, uint64_t,
		 z0 = svpmullt_pair_n_u64 (z1, x0),
		 z0 = svpmullt_pair (z1, x0))

/*
** pmullt_pair_11_u64_tied1:
**	mov	(z[0-9]+\.d), #11
**	pmullt	z0\.q, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_11_u64_tied1, svuint64_t,
		z0 = svpmullt_pair_n_u64 (z0, 11),
		z0 = svpmullt_pair (z0, 11))

/*
** pmullt_pair_11_u64_untied:
**	mov	(z[0-9]+\.d), #11
**	pmullt	z0\.q, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_11_u64_untied, svuint64_t,
		z0 = svpmullt_pair_n_u64 (z1, 11),
		z0 = svpmullt_pair (z1, 11))
