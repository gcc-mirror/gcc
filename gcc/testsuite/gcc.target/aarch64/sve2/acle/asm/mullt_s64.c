/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mullt_s64_tied1:
**	smullt	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_s64_tied1, svint64_t, svint32_t,
		    z0_res = svmullt_s64 (z0, z1),
		    z0_res = svmullt (z0, z1))

/*
** mullt_s64_tied2:
**	smullt	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_s64_tied2, svint64_t, svint32_t,
		    z0_res = svmullt_s64 (z1, z0),
		    z0_res = svmullt (z1, z0))

/*
** mullt_s64_untied:
**	smullt	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_s64_untied, svint64_t, svint32_t,
		    z0_res = svmullt_s64 (z1, z2),
		    z0_res = svmullt (z1, z2))

/*
** mullt_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	smullt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullt_w0_s64_tied1, svint64_t, svint32_t, int32_t,
		     z0_res = svmullt_n_s64 (z0, x0),
		     z0_res = svmullt (z0, x0))

/*
** mullt_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	smullt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullt_w0_s64_untied, svint64_t, svint32_t, int32_t,
		     z0_res = svmullt_n_s64 (z1, x0),
		     z0_res = svmullt (z1, x0))

/*
** mullt_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	smullt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_11_s64_tied1, svint64_t, svint32_t,
		    z0_res = svmullt_n_s64 (z0, 11),
		    z0_res = svmullt (z0, 11))

/*
** mullt_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	smullt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_11_s64_untied, svint64_t, svint32_t,
		    z0_res = svmullt_n_s64 (z1, 11),
		    z0_res = svmullt (z1, 11))
