/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmulh_s16_tied1:
**	sqdmulh	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qdmulh_s16_tied1, svint16_t,
		z0 = svqdmulh_s16 (z0, z1),
		z0 = svqdmulh (z0, z1))

/*
** qdmulh_s16_tied2:
**	sqdmulh	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (qdmulh_s16_tied2, svint16_t,
		z0 = svqdmulh_s16 (z1, z0),
		z0 = svqdmulh (z1, z0))

/*
** qdmulh_s16_untied:
**	sqdmulh	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (qdmulh_s16_untied, svint16_t,
		z0 = svqdmulh_s16 (z1, z2),
		z0 = svqdmulh (z1, z2))

/*
** qdmulh_w0_s16_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqdmulh	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qdmulh_w0_s16_tied1, svint16_t, int16_t,
		 z0 = svqdmulh_n_s16 (z0, x0),
		 z0 = svqdmulh (z0, x0))

/*
** qdmulh_w0_s16_untied:
**	mov	(z[0-9]+\.h), w0
**	sqdmulh	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qdmulh_w0_s16_untied, svint16_t, int16_t,
		 z0 = svqdmulh_n_s16 (z1, x0),
		 z0 = svqdmulh (z1, x0))

/*
** qdmulh_11_s16_tied1:
**	mov	(z[0-9]+\.h), #11
**	sqdmulh	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qdmulh_11_s16_tied1, svint16_t,
		z0 = svqdmulh_n_s16 (z0, 11),
		z0 = svqdmulh (z0, 11))

/*
** qdmulh_11_s16_untied:
**	mov	(z[0-9]+\.h), #11
**	sqdmulh	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qdmulh_11_s16_untied, svint16_t,
		z0 = svqdmulh_n_s16 (z1, 11),
		z0 = svqdmulh (z1, 11))
