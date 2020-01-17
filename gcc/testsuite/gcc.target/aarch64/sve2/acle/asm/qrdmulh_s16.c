/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrdmulh_s16_tied1:
**	sqrdmulh	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_s16_tied1, svint16_t,
		z0 = svqrdmulh_s16 (z0, z1),
		z0 = svqrdmulh (z0, z1))

/*
** qrdmulh_s16_tied2:
**	sqrdmulh	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_s16_tied2, svint16_t,
		z0 = svqrdmulh_s16 (z1, z0),
		z0 = svqrdmulh (z1, z0))

/*
** qrdmulh_s16_untied:
**	sqrdmulh	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_s16_untied, svint16_t,
		z0 = svqrdmulh_s16 (z1, z2),
		z0 = svqrdmulh (z1, z2))

/*
** qrdmulh_w0_s16_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqrdmulh	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmulh_w0_s16_tied1, svint16_t, int16_t,
		 z0 = svqrdmulh_n_s16 (z0, x0),
		 z0 = svqrdmulh (z0, x0))

/*
** qrdmulh_w0_s16_untied:
**	mov	(z[0-9]+\.h), w0
**	sqrdmulh	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmulh_w0_s16_untied, svint16_t, int16_t,
		 z0 = svqrdmulh_n_s16 (z1, x0),
		 z0 = svqrdmulh (z1, x0))

/*
** qrdmulh_11_s16_tied1:
**	mov	(z[0-9]+\.h), #11
**	sqrdmulh	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_11_s16_tied1, svint16_t,
		z0 = svqrdmulh_n_s16 (z0, 11),
		z0 = svqrdmulh (z0, 11))

/*
** qrdmulh_11_s16_untied:
**	mov	(z[0-9]+\.h), #11
**	sqrdmulh	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_11_s16_untied, svint16_t,
		z0 = svqrdmulh_n_s16 (z1, 11),
		z0 = svqrdmulh (z1, 11))
