/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrdmulh_s64_tied1:
**	sqrdmulh	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_s64_tied1, svint64_t,
		z0 = svqrdmulh_s64 (z0, z1),
		z0 = svqrdmulh (z0, z1))

/*
** qrdmulh_s64_tied2:
**	sqrdmulh	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_s64_tied2, svint64_t,
		z0 = svqrdmulh_s64 (z1, z0),
		z0 = svqrdmulh (z1, z0))

/*
** qrdmulh_s64_untied:
**	sqrdmulh	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_s64_untied, svint64_t,
		z0 = svqrdmulh_s64 (z1, z2),
		z0 = svqrdmulh (z1, z2))

/*
** qrdmulh_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	sqrdmulh	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmulh_x0_s64_tied1, svint64_t, int64_t,
		 z0 = svqrdmulh_n_s64 (z0, x0),
		 z0 = svqrdmulh (z0, x0))

/*
** qrdmulh_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
**	sqrdmulh	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmulh_x0_s64_untied, svint64_t, int64_t,
		 z0 = svqrdmulh_n_s64 (z1, x0),
		 z0 = svqrdmulh (z1, x0))

/*
** qrdmulh_11_s64_tied1:
**	mov	(z[0-9]+\.d), #11
**	sqrdmulh	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_11_s64_tied1, svint64_t,
		z0 = svqrdmulh_n_s64 (z0, 11),
		z0 = svqrdmulh (z0, 11))

/*
** qrdmulh_11_s64_untied:
**	mov	(z[0-9]+\.d), #11
**	sqrdmulh	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_11_s64_untied, svint64_t,
		z0 = svqrdmulh_n_s64 (z1, 11),
		z0 = svqrdmulh (z1, 11))
