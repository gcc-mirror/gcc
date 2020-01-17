/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mullb_s16_tied1:
**	smullb	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_s16_tied1, svint16_t, svint8_t,
		    z0_res = svmullb_s16 (z0, z1),
		    z0_res = svmullb (z0, z1))

/*
** mullb_s16_tied2:
**	smullb	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_s16_tied2, svint16_t, svint8_t,
		    z0_res = svmullb_s16 (z1, z0),
		    z0_res = svmullb (z1, z0))

/*
** mullb_s16_untied:
**	smullb	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_s16_untied, svint16_t, svint8_t,
		    z0_res = svmullb_s16 (z1, z2),
		    z0_res = svmullb (z1, z2))

/*
** mullb_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	smullb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullb_w0_s16_tied1, svint16_t, svint8_t, int8_t,
		     z0_res = svmullb_n_s16 (z0, x0),
		     z0_res = svmullb (z0, x0))

/*
** mullb_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	smullb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullb_w0_s16_untied, svint16_t, svint8_t, int8_t,
		     z0_res = svmullb_n_s16 (z1, x0),
		     z0_res = svmullb (z1, x0))

/*
** mullb_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	smullb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_11_s16_tied1, svint16_t, svint8_t,
		    z0_res = svmullb_n_s16 (z0, 11),
		    z0_res = svmullb (z0, 11))

/*
** mullb_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	smullb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_11_s16_untied, svint16_t, svint8_t,
		    z0_res = svmullb_n_s16 (z1, 11),
		    z0_res = svmullb (z1, 11))
