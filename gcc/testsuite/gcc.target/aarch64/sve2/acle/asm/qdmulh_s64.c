/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmulh_s64_tied1:
**	sqdmulh	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qdmulh_s64_tied1, svint64_t,
		z0 = svqdmulh_s64 (z0, z1),
		z0 = svqdmulh (z0, z1))

/*
** qdmulh_s64_tied2:
**	sqdmulh	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qdmulh_s64_tied2, svint64_t,
		z0 = svqdmulh_s64 (z1, z0),
		z0 = svqdmulh (z1, z0))

/*
** qdmulh_s64_untied:
**	sqdmulh	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (qdmulh_s64_untied, svint64_t,
		z0 = svqdmulh_s64 (z1, z2),
		z0 = svqdmulh (z1, z2))

/*
** qdmulh_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	sqdmulh	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qdmulh_x0_s64_tied1, svint64_t, int64_t,
		 z0 = svqdmulh_n_s64 (z0, x0),
		 z0 = svqdmulh (z0, x0))

/*
** qdmulh_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
**	sqdmulh	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qdmulh_x0_s64_untied, svint64_t, int64_t,
		 z0 = svqdmulh_n_s64 (z1, x0),
		 z0 = svqdmulh (z1, x0))

/*
** qdmulh_11_s64_tied1:
**	mov	(z[0-9]+\.d), #11
**	sqdmulh	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qdmulh_11_s64_tied1, svint64_t,
		z0 = svqdmulh_n_s64 (z0, 11),
		z0 = svqdmulh (z0, 11))

/*
** qdmulh_11_s64_untied:
**	mov	(z[0-9]+\.d), #11
**	sqdmulh	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qdmulh_11_s64_untied, svint64_t,
		z0 = svqdmulh_n_s64 (z1, 11),
		z0 = svqdmulh (z1, 11))
