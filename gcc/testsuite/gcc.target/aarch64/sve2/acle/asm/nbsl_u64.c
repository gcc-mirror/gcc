/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** nbsl_u64_tied1:
**	nbsl	z0\.d, z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (nbsl_u64_tied1, svuint64_t,
		z0 = svnbsl_u64 (z0, z1, z2),
		z0 = svnbsl (z0, z1, z2))

/*
** nbsl_u64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	nbsl	z0\.d, z0\.d, \1, z2\.d
**	ret
*/
TEST_UNIFORM_Z (nbsl_u64_tied2, svuint64_t,
		z0 = svnbsl_u64 (z1, z0, z2),
		z0 = svnbsl (z1, z0, z2))

/*
** nbsl_u64_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	nbsl	z0\.d, z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (nbsl_u64_tied3, svuint64_t,
		z0 = svnbsl_u64 (z1, z2, z0),
		z0 = svnbsl (z1, z2, z0))

/*
** nbsl_u64_untied:
**	movprfx	z0, z1
**	nbsl	z0\.d, z0\.d, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (nbsl_u64_untied, svuint64_t,
		z0 = svnbsl_u64 (z1, z2, z3),
		z0 = svnbsl (z1, z2, z3))

/*
** nbsl_x0_u64_tied1:
**	mov	(z[0-9]+\.d), x0
**	nbsl	z0\.d, z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (nbsl_x0_u64_tied1, svuint64_t, uint64_t,
		 z0 = svnbsl_n_u64 (z0, z1, x0),
		 z0 = svnbsl (z0, z1, x0))

/*
** nbsl_x0_u64_tied2:
**	mov	(z[0-9]+\.d), x0
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	nbsl	z0\.d, z0\.d, \2, \1
**	ret
*/
TEST_UNIFORM_ZX (nbsl_x0_u64_tied2, svuint64_t, uint64_t,
		 z0 = svnbsl_n_u64 (z1, z0, x0),
		 z0 = svnbsl (z1, z0, x0))

/*
** nbsl_x0_u64_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	nbsl	z0\.d, z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (nbsl_x0_u64_untied, svuint64_t, uint64_t,
		 z0 = svnbsl_n_u64 (z1, z2, x0),
		 z0 = svnbsl (z1, z2, x0))

/*
** nbsl_11_u64_tied1:
**	mov	(z[0-9]+\.d), #11
**	nbsl	z0\.d, z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (nbsl_11_u64_tied1, svuint64_t,
		z0 = svnbsl_n_u64 (z0, z1, 11),
		z0 = svnbsl (z0, z1, 11))

/*
** nbsl_11_u64_tied2:
**	mov	(z[0-9]+\.d), #11
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	nbsl	z0\.d, z0\.d, \2, \1
**	ret
*/
TEST_UNIFORM_Z (nbsl_11_u64_tied2, svuint64_t,
		z0 = svnbsl_n_u64 (z1, z0, 11),
		z0 = svnbsl (z1, z0, 11))

/*
** nbsl_11_u64_untied:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0, z1
**	nbsl	z0\.d, z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (nbsl_11_u64_untied, svuint64_t,
		z0 = svnbsl_n_u64 (z1, z2, 11),
		z0 = svnbsl (z1, z2, 11))
