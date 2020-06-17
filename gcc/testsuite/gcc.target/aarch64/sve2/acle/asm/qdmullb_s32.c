/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmullb_s32_tied1:
**	sqdmullb	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullb_s32_tied1, svint32_t, svint16_t,
		    z0_res = svqdmullb_s32 (z0, z1),
		    z0_res = svqdmullb (z0, z1))

/*
** qdmullb_s32_tied2:
**	sqdmullb	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullb_s32_tied2, svint32_t, svint16_t,
		    z0_res = svqdmullb_s32 (z1, z0),
		    z0_res = svqdmullb (z1, z0))

/*
** qdmullb_s32_untied:
**	sqdmullb	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullb_s32_untied, svint32_t, svint16_t,
		    z0_res = svqdmullb_s32 (z1, z2),
		    z0_res = svqdmullb (z1, z2))

/*
** qdmullb_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqdmullb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (qdmullb_w0_s32_tied1, svint32_t, svint16_t, int16_t,
		     z0_res = svqdmullb_n_s32 (z0, x0),
		     z0_res = svqdmullb (z0, x0))

/*
** qdmullb_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	sqdmullb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (qdmullb_w0_s32_untied, svint32_t, svint16_t, int16_t,
		     z0_res = svqdmullb_n_s32 (z1, x0),
		     z0_res = svqdmullb (z1, x0))

/*
** qdmullb_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	sqdmullb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullb_11_s32_tied1, svint32_t, svint16_t,
		    z0_res = svqdmullb_n_s32 (z0, 11),
		    z0_res = svqdmullb (z0, 11))

/*
** qdmullb_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	sqdmullb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullb_11_s32_untied, svint32_t, svint16_t,
		    z0_res = svqdmullb_n_s32 (z1, 11),
		    z0_res = svqdmullb (z1, 11))
