/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mullb_s64_tied1:
**	smullb	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_s64_tied1, svint64_t, svint32_t,
		    z0_res = svmullb_s64 (z0, z1),
		    z0_res = svmullb (z0, z1))

/*
** mullb_s64_tied2:
**	smullb	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_s64_tied2, svint64_t, svint32_t,
		    z0_res = svmullb_s64 (z1, z0),
		    z0_res = svmullb (z1, z0))

/*
** mullb_s64_untied:
**	smullb	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_s64_untied, svint64_t, svint32_t,
		    z0_res = svmullb_s64 (z1, z2),
		    z0_res = svmullb (z1, z2))

/*
** mullb_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	smullb	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullb_w0_s64_tied1, svint64_t, svint32_t, int32_t,
		     z0_res = svmullb_n_s64 (z0, x0),
		     z0_res = svmullb (z0, x0))

/*
** mullb_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	smullb	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullb_w0_s64_untied, svint64_t, svint32_t, int32_t,
		     z0_res = svmullb_n_s64 (z1, x0),
		     z0_res = svmullb (z1, x0))

/*
** mullb_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	smullb	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_11_s64_tied1, svint64_t, svint32_t,
		    z0_res = svmullb_n_s64 (z0, 11),
		    z0_res = svmullb (z0, 11))

/*
** mullb_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	smullb	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_11_s64_untied, svint64_t, svint32_t,
		    z0_res = svmullb_n_s64 (z1, 11),
		    z0_res = svmullb (z1, 11))
