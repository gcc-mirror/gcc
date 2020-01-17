/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mullb_u16_tied1:
**	umullb	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svmullb_u16 (z0, z1),
		    z0_res = svmullb (z0, z1))

/*
** mullb_u16_tied2:
**	umullb	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_u16_tied2, svuint16_t, svuint8_t,
		    z0_res = svmullb_u16 (z1, z0),
		    z0_res = svmullb (z1, z0))

/*
** mullb_u16_untied:
**	umullb	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svmullb_u16 (z1, z2),
		    z0_res = svmullb (z1, z2))

/*
** mullb_w0_u16_tied1:
**	mov	(z[0-9]+\.b), w0
**	umullb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullb_w0_u16_tied1, svuint16_t, svuint8_t, uint8_t,
		     z0_res = svmullb_n_u16 (z0, x0),
		     z0_res = svmullb (z0, x0))

/*
** mullb_w0_u16_untied:
**	mov	(z[0-9]+\.b), w0
**	umullb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullb_w0_u16_untied, svuint16_t, svuint8_t, uint8_t,
		     z0_res = svmullb_n_u16 (z1, x0),
		     z0_res = svmullb (z1, x0))

/*
** mullb_11_u16_tied1:
**	mov	(z[0-9]+\.b), #11
**	umullb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_11_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svmullb_n_u16 (z0, 11),
		    z0_res = svmullb (z0, 11))

/*
** mullb_11_u16_untied:
**	mov	(z[0-9]+\.b), #11
**	umullb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_11_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svmullb_n_u16 (z1, 11),
		    z0_res = svmullb (z1, 11))
