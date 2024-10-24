/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** aba_s64_tied1:
**	saba	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (aba_s64_tied1, svint64_t,
		z0 = svaba_s64 (z0, z1, z2),
		z0 = svaba (z0, z1, z2))

/*
** aba_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	saba	z0\.d, \1, z2\.d
**	ret
*/
TEST_UNIFORM_Z (aba_s64_tied2, svint64_t,
		z0 = svaba_s64 (z1, z0, z2),
		z0 = svaba (z1, z0, z2))

/*
** aba_s64_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	saba	z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (aba_s64_tied3, svint64_t,
		z0 = svaba_s64 (z1, z2, z0),
		z0 = svaba (z1, z2, z0))

/*
** aba_s64_untied:
**	movprfx	z0, z1
**	saba	z0\.d, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (aba_s64_untied, svint64_t,
		z0 = svaba_s64 (z1, z2, z3),
		z0 = svaba (z1, z2, z3))

/*
** aba_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	saba	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_x0_s64_tied1, svint64_t, int64_t,
		 z0 = svaba_n_s64 (z0, z1, x0),
		 z0 = svaba (z0, z1, x0))

/*
** aba_x0_s64_tied2:
**	mov	(z[0-9]+\.d), x0
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	saba	z0\.d, \2, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_x0_s64_tied2, svint64_t, int64_t,
		 z0 = svaba_n_s64 (z1, z0, x0),
		 z0 = svaba (z1, z0, x0))

/*
** aba_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	saba	z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_x0_s64_untied, svint64_t, int64_t,
		 z0 = svaba_n_s64 (z1, z2, x0),
		 z0 = svaba (z1, z2, x0))

/*
** aba_11_s64_tied1:
**	mov	(z[0-9]+\.d), #11
**	saba	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_s64_tied1, svint64_t,
		z0 = svaba_n_s64 (z0, z1, 11),
		z0 = svaba (z0, z1, 11))

/*
** aba_11_s64_tied2:
**	mov	(z[0-9]+\.d), #11
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	saba	z0\.d, \2, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_s64_tied2, svint64_t,
		z0 = svaba_n_s64 (z1, z0, 11),
		z0 = svaba (z1, z0, 11))

/*
** aba_11_s64_untied:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0, z1
**	saba	z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_s64_untied, svint64_t,
		z0 = svaba_n_s64 (z1, z2, 11),
		z0 = svaba (z1, z2, 11))

/*
** aba_11_s64_zeroop1n:
**	ptrue	(p[0-7])\.b, all
**	mov	z0\.d, #11
**	sabd	z0\.d, \1/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (aba_11_s64_zeroop1n, svint64_t,
		z0 = svaba_n_s64 (svdup_s64 (0), z1, 11),
		z0 = svaba (svdup_s64 (0), z1, 11))

/*
** aba_11_s64_zeroop1:
**	ptrue	(p[0-7])\.b, all
**	mov	z0\.d, #11
**	sabd	z0\.d, \1/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (aba_11_s64_zeroop1, svint64_t,
		z0 = svaba_s64 (svdup_s64 (0), z1, svdup_s64 (11)),
		z0 = svaba (svdup_s64 (0), z1, svdup_s64 (11)))
