/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmullt_s16_tied1:
**	sqdmullt	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_s16_tied1, svint16_t, svint8_t,
		    z0_res = svqdmullt_s16 (z0, z1),
		    z0_res = svqdmullt (z0, z1))

/*
** qdmullt_s16_tied2:
**	sqdmullt	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_s16_tied2, svint16_t, svint8_t,
		    z0_res = svqdmullt_s16 (z1, z0),
		    z0_res = svqdmullt (z1, z0))

/*
** qdmullt_s16_untied:
**	sqdmullt	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_s16_untied, svint16_t, svint8_t,
		    z0_res = svqdmullt_s16 (z1, z2),
		    z0_res = svqdmullt (z1, z2))

/*
** qdmullt_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	sqdmullt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (qdmullt_w0_s16_tied1, svint16_t, svint8_t, int8_t,
		     z0_res = svqdmullt_n_s16 (z0, x0),
		     z0_res = svqdmullt (z0, x0))

/*
** qdmullt_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	sqdmullt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (qdmullt_w0_s16_untied, svint16_t, svint8_t, int8_t,
		     z0_res = svqdmullt_n_s16 (z1, x0),
		     z0_res = svqdmullt (z1, x0))

/*
** qdmullt_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	sqdmullt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_11_s16_tied1, svint16_t, svint8_t,
		    z0_res = svqdmullt_n_s16 (z0, 11),
		    z0_res = svqdmullt (z0, 11))

/*
** qdmullt_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	sqdmullt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_11_s16_untied, svint16_t, svint8_t,
		    z0_res = svqdmullt_n_s16 (z1, 11),
		    z0_res = svqdmullt (z1, 11))
