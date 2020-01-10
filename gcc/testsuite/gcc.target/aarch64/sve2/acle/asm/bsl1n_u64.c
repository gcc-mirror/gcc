/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** bsl1n_u64_tied1:
**	bsl1n	z0\.d, z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bsl1n_u64_tied1, svuint64_t,
		z0 = svbsl1n_u64 (z0, z1, z2),
		z0 = svbsl1n (z0, z1, z2))

/*
** bsl1n_u64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bsl1n	z0\.d, z0\.d, \1, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bsl1n_u64_tied2, svuint64_t,
		z0 = svbsl1n_u64 (z1, z0, z2),
		z0 = svbsl1n (z1, z0, z2))

/*
** bsl1n_u64_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bsl1n	z0\.d, z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bsl1n_u64_tied3, svuint64_t,
		z0 = svbsl1n_u64 (z1, z2, z0),
		z0 = svbsl1n (z1, z2, z0))

/*
** bsl1n_u64_untied:
**	movprfx	z0, z1
**	bsl1n	z0\.d, z0\.d, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (bsl1n_u64_untied, svuint64_t,
		z0 = svbsl1n_u64 (z1, z2, z3),
		z0 = svbsl1n (z1, z2, z3))

/*
** bsl1n_x0_u64_tied1:
**	mov	(z[0-9]+\.d), x0
**	bsl1n	z0\.d, z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bsl1n_x0_u64_tied1, svuint64_t, uint64_t,
		 z0 = svbsl1n_n_u64 (z0, z1, x0),
		 z0 = svbsl1n (z0, z1, x0))

/*
** bsl1n_x0_u64_tied2:
**	mov	(z[0-9]+\.d), x0
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bsl1n	z0\.d, z0\.d, \2, \1
**	ret
*/
TEST_UNIFORM_ZX (bsl1n_x0_u64_tied2, svuint64_t, uint64_t,
		 z0 = svbsl1n_n_u64 (z1, z0, x0),
		 z0 = svbsl1n (z1, z0, x0))

/*
** bsl1n_x0_u64_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	bsl1n	z0\.d, z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bsl1n_x0_u64_untied, svuint64_t, uint64_t,
		 z0 = svbsl1n_n_u64 (z1, z2, x0),
		 z0 = svbsl1n (z1, z2, x0))

/*
** bsl1n_11_u64_tied1:
**	mov	(z[0-9]+\.d), #11
**	bsl1n	z0\.d, z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bsl1n_11_u64_tied1, svuint64_t,
		z0 = svbsl1n_n_u64 (z0, z1, 11),
		z0 = svbsl1n (z0, z1, 11))

/*
** bsl1n_11_u64_tied2:
**	mov	(z[0-9]+\.d), #11
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bsl1n	z0\.d, z0\.d, \2, \1
**	ret
*/
TEST_UNIFORM_Z (bsl1n_11_u64_tied2, svuint64_t,
		z0 = svbsl1n_n_u64 (z1, z0, 11),
		z0 = svbsl1n (z1, z0, 11))

/*
** bsl1n_11_u64_untied:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0, z1
**	bsl1n	z0\.d, z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bsl1n_11_u64_untied, svuint64_t,
		z0 = svbsl1n_n_u64 (z1, z2, 11),
		z0 = svbsl1n (z1, z2, 11))
