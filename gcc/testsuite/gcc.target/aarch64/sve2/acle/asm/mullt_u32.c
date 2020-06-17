/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mullt_u32_tied1:
**	umullt	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svmullt_u32 (z0, z1),
		    z0_res = svmullt (z0, z1))

/*
** mullt_u32_tied2:
**	umullt	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_u32_tied2, svuint32_t, svuint16_t,
		    z0_res = svmullt_u32 (z1, z0),
		    z0_res = svmullt (z1, z0))

/*
** mullt_u32_untied:
**	umullt	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svmullt_u32 (z1, z2),
		    z0_res = svmullt (z1, z2))

/*
** mullt_w0_u32_tied1:
**	mov	(z[0-9]+\.h), w0
**	umullt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullt_w0_u32_tied1, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svmullt_n_u32 (z0, x0),
		     z0_res = svmullt (z0, x0))

/*
** mullt_w0_u32_untied:
**	mov	(z[0-9]+\.h), w0
**	umullt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullt_w0_u32_untied, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svmullt_n_u32 (z1, x0),
		     z0_res = svmullt (z1, x0))

/*
** mullt_11_u32_tied1:
**	mov	(z[0-9]+\.h), #11
**	umullt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_11_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svmullt_n_u32 (z0, 11),
		    z0_res = svmullt (z0, 11))

/*
** mullt_11_u32_untied:
**	mov	(z[0-9]+\.h), #11
**	umullt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_11_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svmullt_n_u32 (z1, 11),
		    z0_res = svmullt (z1, 11))
