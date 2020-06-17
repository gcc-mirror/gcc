/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mullt_s32_tied1:
**	smullt	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_s32_tied1, svint32_t, svint16_t,
		    z0_res = svmullt_s32 (z0, z1),
		    z0_res = svmullt (z0, z1))

/*
** mullt_s32_tied2:
**	smullt	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_s32_tied2, svint32_t, svint16_t,
		    z0_res = svmullt_s32 (z1, z0),
		    z0_res = svmullt (z1, z0))

/*
** mullt_s32_untied:
**	smullt	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_s32_untied, svint32_t, svint16_t,
		    z0_res = svmullt_s32 (z1, z2),
		    z0_res = svmullt (z1, z2))

/*
** mullt_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	smullt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullt_w0_s32_tied1, svint32_t, svint16_t, int16_t,
		     z0_res = svmullt_n_s32 (z0, x0),
		     z0_res = svmullt (z0, x0))

/*
** mullt_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	smullt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullt_w0_s32_untied, svint32_t, svint16_t, int16_t,
		     z0_res = svmullt_n_s32 (z1, x0),
		     z0_res = svmullt (z1, x0))

/*
** mullt_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	smullt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_11_s32_tied1, svint32_t, svint16_t,
		    z0_res = svmullt_n_s32 (z0, 11),
		    z0_res = svmullt (z0, 11))

/*
** mullt_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	smullt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_11_s32_untied, svint32_t, svint16_t,
		    z0_res = svmullt_n_s32 (z1, 11),
		    z0_res = svmullt (z1, 11))
