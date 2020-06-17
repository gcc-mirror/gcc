/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrdmulh_s8_tied1:
**	sqrdmulh	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_s8_tied1, svint8_t,
		z0 = svqrdmulh_s8 (z0, z1),
		z0 = svqrdmulh (z0, z1))

/*
** qrdmulh_s8_tied2:
**	sqrdmulh	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_s8_tied2, svint8_t,
		z0 = svqrdmulh_s8 (z1, z0),
		z0 = svqrdmulh (z1, z0))

/*
** qrdmulh_s8_untied:
**	sqrdmulh	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_s8_untied, svint8_t,
		z0 = svqrdmulh_s8 (z1, z2),
		z0 = svqrdmulh (z1, z2))

/*
** qrdmulh_w0_s8_tied1:
**	mov	(z[0-9]+\.b), w0
**	sqrdmulh	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmulh_w0_s8_tied1, svint8_t, int8_t,
		 z0 = svqrdmulh_n_s8 (z0, x0),
		 z0 = svqrdmulh (z0, x0))

/*
** qrdmulh_w0_s8_untied:
**	mov	(z[0-9]+\.b), w0
**	sqrdmulh	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmulh_w0_s8_untied, svint8_t, int8_t,
		 z0 = svqrdmulh_n_s8 (z1, x0),
		 z0 = svqrdmulh (z1, x0))

/*
** qrdmulh_11_s8_tied1:
**	mov	(z[0-9]+\.b), #11
**	sqrdmulh	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_11_s8_tied1, svint8_t,
		z0 = svqrdmulh_n_s8 (z0, 11),
		z0 = svqrdmulh (z0, 11))

/*
** qrdmulh_11_s8_untied:
**	mov	(z[0-9]+\.b), #11
**	sqrdmulh	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_11_s8_untied, svint8_t,
		z0 = svqrdmulh_n_s8 (z1, 11),
		z0 = svqrdmulh (z1, 11))
