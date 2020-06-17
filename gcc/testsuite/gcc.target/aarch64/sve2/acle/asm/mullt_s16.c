/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mullt_s16_tied1:
**	smullt	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_s16_tied1, svint16_t, svint8_t,
		    z0_res = svmullt_s16 (z0, z1),
		    z0_res = svmullt (z0, z1))

/*
** mullt_s16_tied2:
**	smullt	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_s16_tied2, svint16_t, svint8_t,
		    z0_res = svmullt_s16 (z1, z0),
		    z0_res = svmullt (z1, z0))

/*
** mullt_s16_untied:
**	smullt	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_s16_untied, svint16_t, svint8_t,
		    z0_res = svmullt_s16 (z1, z2),
		    z0_res = svmullt (z1, z2))

/*
** mullt_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	smullt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullt_w0_s16_tied1, svint16_t, svint8_t, int8_t,
		     z0_res = svmullt_n_s16 (z0, x0),
		     z0_res = svmullt (z0, x0))

/*
** mullt_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	smullt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullt_w0_s16_untied, svint16_t, svint8_t, int8_t,
		     z0_res = svmullt_n_s16 (z1, x0),
		     z0_res = svmullt (z1, x0))

/*
** mullt_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	smullt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_11_s16_tied1, svint16_t, svint8_t,
		    z0_res = svmullt_n_s16 (z0, 11),
		    z0_res = svmullt (z0, 11))

/*
** mullt_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	smullt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_11_s16_untied, svint16_t, svint8_t,
		    z0_res = svmullt_n_s16 (z1, 11),
		    z0_res = svmullt (z1, 11))
