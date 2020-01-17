/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mullb_u32_tied1:
**	umullb	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svmullb_u32 (z0, z1),
		    z0_res = svmullb (z0, z1))

/*
** mullb_u32_tied2:
**	umullb	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_u32_tied2, svuint32_t, svuint16_t,
		    z0_res = svmullb_u32 (z1, z0),
		    z0_res = svmullb (z1, z0))

/*
** mullb_u32_untied:
**	umullb	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svmullb_u32 (z1, z2),
		    z0_res = svmullb (z1, z2))

/*
** mullb_w0_u32_tied1:
**	mov	(z[0-9]+\.h), w0
**	umullb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullb_w0_u32_tied1, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svmullb_n_u32 (z0, x0),
		     z0_res = svmullb (z0, x0))

/*
** mullb_w0_u32_untied:
**	mov	(z[0-9]+\.h), w0
**	umullb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (mullb_w0_u32_untied, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svmullb_n_u32 (z1, x0),
		     z0_res = svmullb (z1, x0))

/*
** mullb_11_u32_tied1:
**	mov	(z[0-9]+\.h), #11
**	umullb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_11_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svmullb_n_u32 (z0, 11),
		    z0_res = svmullb (z0, 11))

/*
** mullb_11_u32_untied:
**	mov	(z[0-9]+\.h), #11
**	umullb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (mullb_11_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svmullb_n_u32 (z1, 11),
		    z0_res = svmullb (z1, 11))
