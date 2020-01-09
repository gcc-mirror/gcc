/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subwb_u32_tied1:
**	usubwb	z0\.s, z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (subwb_u32_tied1, svuint32_t, svuint16_t,
	     z0 = svsubwb_u32 (z0, z4),
	     z0 = svsubwb (z0, z4))

/*
** subwb_u32_tied2:
**	usubwb	z0\.s, z4\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (subwb_u32_tied2, svuint32_t, svuint16_t,
		 z0_res = svsubwb_u32 (z4, z0),
		 z0_res = svsubwb (z4, z0))

/*
** subwb_u32_untied:
**	usubwb	z0\.s, z1\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (subwb_u32_untied, svuint32_t, svuint16_t,
	     z0 = svsubwb_u32 (z1, z4),
	     z0 = svsubwb (z1, z4))

/*
** subwb_w0_u32_tied1:
**	mov	(z[0-9]+\.h), w0
**	usubwb	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (subwb_w0_u32_tied1, svuint32_t, uint16_t,
		 z0 = svsubwb_n_u32 (z0, x0),
		 z0 = svsubwb (z0, x0))

/*
** subwb_w0_u32_untied:
**	mov	(z[0-9]+\.h), w0
**	usubwb	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (subwb_w0_u32_untied, svuint32_t, uint16_t,
		 z0 = svsubwb_n_u32 (z1, x0),
		 z0 = svsubwb (z1, x0))

/*
** subwb_11_u32_tied1:
**	mov	(z[0-9]+\.h), #11
**	usubwb	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (subwb_11_u32_tied1, svuint32_t,
		z0 = svsubwb_n_u32 (z0, 11),
		z0 = svsubwb (z0, 11))

/*
** subwb_11_u32_untied:
**	mov	(z[0-9]+\.h), #11
**	usubwb	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (subwb_11_u32_untied, svuint32_t,
		z0 = svsubwb_n_u32 (z1, 11),
		z0 = svsubwb (z1, 11))
