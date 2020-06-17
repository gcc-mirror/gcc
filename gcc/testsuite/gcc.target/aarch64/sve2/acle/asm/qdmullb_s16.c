/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmullb_s16_tied1:
**	sqdmullb	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullb_s16_tied1, svint16_t, svint8_t,
		    z0_res = svqdmullb_s16 (z0, z1),
		    z0_res = svqdmullb (z0, z1))

/*
** qdmullb_s16_tied2:
**	sqdmullb	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullb_s16_tied2, svint16_t, svint8_t,
		    z0_res = svqdmullb_s16 (z1, z0),
		    z0_res = svqdmullb (z1, z0))

/*
** qdmullb_s16_untied:
**	sqdmullb	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullb_s16_untied, svint16_t, svint8_t,
		    z0_res = svqdmullb_s16 (z1, z2),
		    z0_res = svqdmullb (z1, z2))

/*
** qdmullb_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	sqdmullb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (qdmullb_w0_s16_tied1, svint16_t, svint8_t, int8_t,
		     z0_res = svqdmullb_n_s16 (z0, x0),
		     z0_res = svqdmullb (z0, x0))

/*
** qdmullb_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	sqdmullb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (qdmullb_w0_s16_untied, svint16_t, svint8_t, int8_t,
		     z0_res = svqdmullb_n_s16 (z1, x0),
		     z0_res = svqdmullb (z1, x0))

/*
** qdmullb_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	sqdmullb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullb_11_s16_tied1, svint16_t, svint8_t,
		    z0_res = svqdmullb_n_s16 (z0, 11),
		    z0_res = svqdmullb (z0, 11))

/*
** qdmullb_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	sqdmullb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullb_11_s16_untied, svint16_t, svint8_t,
		    z0_res = svqdmullb_n_s16 (z1, 11),
		    z0_res = svqdmullb (z1, 11))
