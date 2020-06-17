/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addwt_u32_tied1:
**	uaddwt	z0\.s, z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (addwt_u32_tied1, svuint32_t, svuint16_t,
	     z0 = svaddwt_u32 (z0, z4),
	     z0 = svaddwt (z0, z4))

/*
** addwt_u32_tied2:
**	uaddwt	z0\.s, z4\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (addwt_u32_tied2, svuint32_t, svuint16_t,
		 z0_res = svaddwt_u32 (z4, z0),
		 z0_res = svaddwt (z4, z0))

/*
** addwt_u32_untied:
**	uaddwt	z0\.s, z1\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (addwt_u32_untied, svuint32_t, svuint16_t,
	     z0 = svaddwt_u32 (z1, z4),
	     z0 = svaddwt (z1, z4))

/*
** addwt_w0_u32_tied1:
**	mov	(z[0-9]+\.h), w0
**	uaddwt	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (addwt_w0_u32_tied1, svuint32_t, uint16_t,
		 z0 = svaddwt_n_u32 (z0, x0),
		 z0 = svaddwt (z0, x0))

/*
** addwt_w0_u32_untied:
**	mov	(z[0-9]+\.h), w0
**	uaddwt	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (addwt_w0_u32_untied, svuint32_t, uint16_t,
		 z0 = svaddwt_n_u32 (z1, x0),
		 z0 = svaddwt (z1, x0))

/*
** addwt_11_u32_tied1:
**	mov	(z[0-9]+\.h), #11
**	uaddwt	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (addwt_11_u32_tied1, svuint32_t,
		z0 = svaddwt_n_u32 (z0, 11),
		z0 = svaddwt (z0, 11))

/*
** addwt_11_u32_untied:
**	mov	(z[0-9]+\.h), #11
**	uaddwt	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (addwt_11_u32_untied, svuint32_t,
		z0 = svaddwt_n_u32 (z1, 11),
		z0 = svaddwt (z1, 11))
