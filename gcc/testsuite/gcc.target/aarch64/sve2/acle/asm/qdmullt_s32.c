/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmullt_s32_tied1:
**	sqdmullt	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_s32_tied1, svint32_t, svint16_t,
		    z0_res = svqdmullt_s32 (z0, z1),
		    z0_res = svqdmullt (z0, z1))

/*
** qdmullt_s32_tied2:
**	sqdmullt	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_s32_tied2, svint32_t, svint16_t,
		    z0_res = svqdmullt_s32 (z1, z0),
		    z0_res = svqdmullt (z1, z0))

/*
** qdmullt_s32_untied:
**	sqdmullt	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_s32_untied, svint32_t, svint16_t,
		    z0_res = svqdmullt_s32 (z1, z2),
		    z0_res = svqdmullt (z1, z2))

/*
** qdmullt_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqdmullt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (qdmullt_w0_s32_tied1, svint32_t, svint16_t, int16_t,
		     z0_res = svqdmullt_n_s32 (z0, x0),
		     z0_res = svqdmullt (z0, x0))

/*
** qdmullt_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	sqdmullt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (qdmullt_w0_s32_untied, svint32_t, svint16_t, int16_t,
		     z0_res = svqdmullt_n_s32 (z1, x0),
		     z0_res = svqdmullt (z1, x0))

/*
** qdmullt_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	sqdmullt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_11_s32_tied1, svint32_t, svint16_t,
		    z0_res = svqdmullt_n_s32 (z0, 11),
		    z0_res = svqdmullt (z0, 11))

/*
** qdmullt_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	sqdmullt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_11_s32_untied, svint32_t, svint16_t,
		    z0_res = svqdmullt_n_s32 (z1, 11),
		    z0_res = svqdmullt (z1, 11))
