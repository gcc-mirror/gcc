/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addwb_u32_tied1:
**	uaddwb	z0\.s, z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (addwb_u32_tied1, svuint32_t, svuint16_t,
	     z0 = svaddwb_u32 (z0, z4),
	     z0 = svaddwb (z0, z4))

/*
** addwb_u32_tied2:
**	uaddwb	z0\.s, z4\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (addwb_u32_tied2, svuint32_t, svuint16_t,
		 z0_res = svaddwb_u32 (z4, z0),
		 z0_res = svaddwb (z4, z0))

/*
** addwb_u32_untied:
**	uaddwb	z0\.s, z1\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (addwb_u32_untied, svuint32_t, svuint16_t,
	     z0 = svaddwb_u32 (z1, z4),
	     z0 = svaddwb (z1, z4))

/*
** addwb_w0_u32_tied1:
**	mov	(z[0-9]+\.h), w0
**	uaddwb	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (addwb_w0_u32_tied1, svuint32_t, uint16_t,
		 z0 = svaddwb_n_u32 (z0, x0),
		 z0 = svaddwb (z0, x0))

/*
** addwb_w0_u32_untied:
**	mov	(z[0-9]+\.h), w0
**	uaddwb	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (addwb_w0_u32_untied, svuint32_t, uint16_t,
		 z0 = svaddwb_n_u32 (z1, x0),
		 z0 = svaddwb (z1, x0))

/*
** addwb_11_u32_tied1:
**	mov	(z[0-9]+\.h), #11
**	uaddwb	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (addwb_11_u32_tied1, svuint32_t,
		z0 = svaddwb_n_u32 (z0, 11),
		z0 = svaddwb (z0, 11))

/*
** addwb_11_u32_untied:
**	mov	(z[0-9]+\.h), #11
**	uaddwb	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (addwb_11_u32_untied, svuint32_t,
		z0 = svaddwb_n_u32 (z1, 11),
		z0 = svaddwb (z1, 11))
