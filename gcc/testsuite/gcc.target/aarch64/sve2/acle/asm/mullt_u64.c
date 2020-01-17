/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mullt_u64_tied1:
**	umullt	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svmullt_u64 (z0, z1),
		    z0_res = svmullt (z0, z1))

/*
** mullt_u64_tied2:
**	umullt	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_u64_tied2, svuint64_t, svuint32_t,
		    z0_res = svmullt_u64 (z1, z0),
		    z0_res = svmullt (z1, z0))

/*
** mullt_u64_untied:
**	umullt	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svmullt_u64 (z1, z2),
		    z0_res = svmullt (z1, z2))

/*
** mullt_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	umullt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullt_w0_u64_tied1, svuint64_t, svuint32_t, uint32_t,
		     z0_res = svmullt_n_u64 (z0, x0),
		     z0_res = svmullt (z0, x0))

/*
** mullt_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	umullt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullt_w0_u64_untied, svuint64_t, svuint32_t, uint32_t,
		     z0_res = svmullt_n_u64 (z1, x0),
		     z0_res = svmullt (z1, x0))

/*
** mullt_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	umullt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_11_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svmullt_n_u64 (z0, 11),
		    z0_res = svmullt (z0, 11))

/*
** mullt_11_u64_untied:
**	mov	(z[0-9]+\.s), #11
**	umullt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_11_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svmullt_n_u64 (z1, 11),
		    z0_res = svmullt (z1, 11))
