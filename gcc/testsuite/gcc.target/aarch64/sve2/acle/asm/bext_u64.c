/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-bitperm"

/*
** bext_u64_tied1:
**	bext	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (bext_u64_tied1, svuint64_t,
		z0 = svbext_u64 (z0, z1),
		z0 = svbext (z0, z1))

/*
** bext_u64_tied2:
**	bext	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (bext_u64_tied2, svuint64_t,
		z0 = svbext_u64 (z1, z0),
		z0 = svbext (z1, z0))

/*
** bext_u64_untied:
**	bext	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bext_u64_untied, svuint64_t,
		z0 = svbext_u64 (z1, z2),
		z0 = svbext (z1, z2))

/*
** bext_x0_u64_tied1:
**	mov	(z[0-9]+\.d), x0
**	bext	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bext_x0_u64_tied1, svuint64_t, uint64_t,
		 z0 = svbext_n_u64 (z0, x0),
		 z0 = svbext (z0, x0))

/*
** bext_x0_u64_untied:
**	mov	(z[0-9]+\.d), x0
**	bext	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bext_x0_u64_untied, svuint64_t, uint64_t,
		 z0 = svbext_n_u64 (z1, x0),
		 z0 = svbext (z1, x0))

/*
** bext_11_u64_tied1:
**	mov	(z[0-9]+\.d), #11
**	bext	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bext_11_u64_tied1, svuint64_t,
		z0 = svbext_n_u64 (z0, 11),
		z0 = svbext (z0, 11))

/*
** bext_11_u64_untied:
**	mov	(z[0-9]+\.d), #11
**	bext	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bext_11_u64_untied, svuint64_t,
		z0 = svbext_n_u64 (z1, 11),
		z0 = svbext (z1, 11))
