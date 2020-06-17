/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmullt_s64_tied1:
**	sqdmullt	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_s64_tied1, svint64_t, svint32_t,
		    z0_res = svqdmullt_s64 (z0, z1),
		    z0_res = svqdmullt (z0, z1))

/*
** qdmullt_s64_tied2:
**	sqdmullt	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_s64_tied2, svint64_t, svint32_t,
		    z0_res = svqdmullt_s64 (z1, z0),
		    z0_res = svqdmullt (z1, z0))

/*
** qdmullt_s64_untied:
**	sqdmullt	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_s64_untied, svint64_t, svint32_t,
		    z0_res = svqdmullt_s64 (z1, z2),
		    z0_res = svqdmullt (z1, z2))

/*
** qdmullt_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	sqdmullt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (qdmullt_w0_s64_tied1, svint64_t, svint32_t, int32_t,
		     z0_res = svqdmullt_n_s64 (z0, x0),
		     z0_res = svqdmullt (z0, x0))

/*
** qdmullt_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	sqdmullt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (qdmullt_w0_s64_untied, svint64_t, svint32_t, int32_t,
		     z0_res = svqdmullt_n_s64 (z1, x0),
		     z0_res = svqdmullt (z1, x0))

/*
** qdmullt_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	sqdmullt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_11_s64_tied1, svint64_t, svint32_t,
		    z0_res = svqdmullt_n_s64 (z0, 11),
		    z0_res = svqdmullt (z0, 11))

/*
** qdmullt_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	sqdmullt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_11_s64_untied, svint64_t, svint32_t,
		    z0_res = svqdmullt_n_s64 (z1, 11),
		    z0_res = svqdmullt (z1, 11))
