/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmulh_s8_tied1:
**	sqdmulh	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qdmulh_s8_tied1, svint8_t,
		z0 = svqdmulh_s8 (z0, z1),
		z0 = svqdmulh (z0, z1))

/*
** qdmulh_s8_tied2:
**	sqdmulh	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qdmulh_s8_tied2, svint8_t,
		z0 = svqdmulh_s8 (z1, z0),
		z0 = svqdmulh (z1, z0))

/*
** qdmulh_s8_untied:
**	sqdmulh	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (qdmulh_s8_untied, svint8_t,
		z0 = svqdmulh_s8 (z1, z2),
		z0 = svqdmulh (z1, z2))

/*
** qdmulh_w0_s8_tied1:
**	mov	(z[0-9]+\.b), w0
**	sqdmulh	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qdmulh_w0_s8_tied1, svint8_t, int8_t,
		 z0 = svqdmulh_n_s8 (z0, x0),
		 z0 = svqdmulh (z0, x0))

/*
** qdmulh_w0_s8_untied:
**	mov	(z[0-9]+\.b), w0
**	sqdmulh	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qdmulh_w0_s8_untied, svint8_t, int8_t,
		 z0 = svqdmulh_n_s8 (z1, x0),
		 z0 = svqdmulh (z1, x0))

/*
** qdmulh_11_s8_tied1:
**	mov	(z[0-9]+\.b), #11
**	sqdmulh	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qdmulh_11_s8_tied1, svint8_t,
		z0 = svqdmulh_n_s8 (z0, 11),
		z0 = svqdmulh (z0, 11))

/*
** qdmulh_11_s8_untied:
**	mov	(z[0-9]+\.b), #11
**	sqdmulh	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (qdmulh_11_s8_untied, svint8_t,
		z0 = svqdmulh_n_s8 (z1, 11),
		z0 = svqdmulh (z1, 11))
