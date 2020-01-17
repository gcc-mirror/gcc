/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmulh_s32_tied1:
**	sqdmulh	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qdmulh_s32_tied1, svint32_t,
		z0 = svqdmulh_s32 (z0, z1),
		z0 = svqdmulh (z0, z1))

/*
** qdmulh_s32_tied2:
**	sqdmulh	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qdmulh_s32_tied2, svint32_t,
		z0 = svqdmulh_s32 (z1, z0),
		z0 = svqdmulh (z1, z0))

/*
** qdmulh_s32_untied:
**	sqdmulh	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (qdmulh_s32_untied, svint32_t,
		z0 = svqdmulh_s32 (z1, z2),
		z0 = svqdmulh (z1, z2))

/*
** qdmulh_w0_s32_tied1:
**	mov	(z[0-9]+\.s), w0
**	sqdmulh	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qdmulh_w0_s32_tied1, svint32_t, int32_t,
		 z0 = svqdmulh_n_s32 (z0, x0),
		 z0 = svqdmulh (z0, x0))

/*
** qdmulh_w0_s32_untied:
**	mov	(z[0-9]+\.s), w0
**	sqdmulh	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qdmulh_w0_s32_untied, svint32_t, int32_t,
		 z0 = svqdmulh_n_s32 (z1, x0),
		 z0 = svqdmulh (z1, x0))

/*
** qdmulh_11_s32_tied1:
**	mov	(z[0-9]+\.s), #11
**	sqdmulh	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qdmulh_11_s32_tied1, svint32_t,
		z0 = svqdmulh_n_s32 (z0, 11),
		z0 = svqdmulh (z0, 11))

/*
** qdmulh_11_s32_untied:
**	mov	(z[0-9]+\.s), #11
**	sqdmulh	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qdmulh_11_s32_untied, svint32_t,
		z0 = svqdmulh_n_s32 (z1, 11),
		z0 = svqdmulh (z1, 11))
