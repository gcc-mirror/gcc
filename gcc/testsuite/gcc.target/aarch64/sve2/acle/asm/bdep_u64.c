/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-bitperm"

/*
** bdep_u64_tied1:
**	bdep	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (bdep_u64_tied1, svuint64_t,
		z0 = svbdep_u64 (z0, z1),
		z0 = svbdep (z0, z1))

/*
** bdep_u64_tied2:
**	bdep	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (bdep_u64_tied2, svuint64_t,
		z0 = svbdep_u64 (z1, z0),
		z0 = svbdep (z1, z0))

/*
** bdep_u64_untied:
**	bdep	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bdep_u64_untied, svuint64_t,
		z0 = svbdep_u64 (z1, z2),
		z0 = svbdep (z1, z2))

/*
** bdep_x0_u64_tied1:
**	mov	(z[0-9]+\.d), x0
**	bdep	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bdep_x0_u64_tied1, svuint64_t, uint64_t,
		 z0 = svbdep_n_u64 (z0, x0),
		 z0 = svbdep (z0, x0))

/*
** bdep_x0_u64_untied:
**	mov	(z[0-9]+\.d), x0
**	bdep	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bdep_x0_u64_untied, svuint64_t, uint64_t,
		 z0 = svbdep_n_u64 (z1, x0),
		 z0 = svbdep (z1, x0))

/*
** bdep_11_u64_tied1:
**	mov	(z[0-9]+\.d), #11
**	bdep	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bdep_11_u64_tied1, svuint64_t,
		z0 = svbdep_n_u64 (z0, 11),
		z0 = svbdep (z0, 11))

/*
** bdep_11_u64_untied:
**	mov	(z[0-9]+\.d), #11
**	bdep	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bdep_11_u64_untied, svuint64_t,
		z0 = svbdep_n_u64 (z1, 11),
		z0 = svbdep (z1, 11))
