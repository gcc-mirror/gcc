/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mullt_u16_tied1:
**	umullt	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svmullt_u16 (z0, z1),
		    z0_res = svmullt (z0, z1))

/*
** mullt_u16_tied2:
**	umullt	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_u16_tied2, svuint16_t, svuint8_t,
		    z0_res = svmullt_u16 (z1, z0),
		    z0_res = svmullt (z1, z0))

/*
** mullt_u16_untied:
**	umullt	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svmullt_u16 (z1, z2),
		    z0_res = svmullt (z1, z2))

/*
** mullt_w0_u16_tied1:
**	mov	(z[0-9]+\.b), w0
**	umullt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullt_w0_u16_tied1, svuint16_t, svuint8_t, uint8_t,
		     z0_res = svmullt_n_u16 (z0, x0),
		     z0_res = svmullt (z0, x0))

/*
** mullt_w0_u16_untied:
**	mov	(z[0-9]+\.b), w0
**	umullt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullt_w0_u16_untied, svuint16_t, svuint8_t, uint8_t,
		     z0_res = svmullt_n_u16 (z1, x0),
		     z0_res = svmullt (z1, x0))

/*
** mullt_11_u16_tied1:
**	mov	(z[0-9]+\.b), #11
**	umullt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_11_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svmullt_n_u16 (z0, 11),
		    z0_res = svmullt (z0, 11))

/*
** mullt_11_u16_untied:
**	mov	(z[0-9]+\.b), #11
**	umullt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_11_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svmullt_n_u16 (z1, 11),
		    z0_res = svmullt (z1, 11))
