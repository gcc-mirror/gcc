/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subwt_u32_tied1:
**	usubwt	z0\.s, z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (subwt_u32_tied1, svuint32_t, svuint16_t,
	     z0 = svsubwt_u32 (z0, z4),
	     z0 = svsubwt (z0, z4))

/*
** subwt_u32_tied2:
**	usubwt	z0\.s, z4\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (subwt_u32_tied2, svuint32_t, svuint16_t,
		 z0_res = svsubwt_u32 (z4, z0),
		 z0_res = svsubwt (z4, z0))

/*
** subwt_u32_untied:
**	usubwt	z0\.s, z1\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (subwt_u32_untied, svuint32_t, svuint16_t,
	     z0 = svsubwt_u32 (z1, z4),
	     z0 = svsubwt (z1, z4))

/*
** subwt_w0_u32_tied1:
**	mov	(z[0-9]+\.h), w0
**	usubwt	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (subwt_w0_u32_tied1, svuint32_t, uint16_t,
		 z0 = svsubwt_n_u32 (z0, x0),
		 z0 = svsubwt (z0, x0))

/*
** subwt_w0_u32_untied:
**	mov	(z[0-9]+\.h), w0
**	usubwt	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (subwt_w0_u32_untied, svuint32_t, uint16_t,
		 z0 = svsubwt_n_u32 (z1, x0),
		 z0 = svsubwt (z1, x0))

/*
** subwt_11_u32_tied1:
**	mov	(z[0-9]+\.h), #11
**	usubwt	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (subwt_11_u32_tied1, svuint32_t,
		z0 = svsubwt_n_u32 (z0, 11),
		z0 = svsubwt (z0, 11))

/*
** subwt_11_u32_untied:
**	mov	(z[0-9]+\.h), #11
**	usubwt	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (subwt_11_u32_untied, svuint32_t,
		z0 = svsubwt_n_u32 (z1, 11),
		z0 = svsubwt (z1, 11))
