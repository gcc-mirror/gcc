/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addwb_s32_tied1:
**	saddwb	z0\.s, z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (addwb_s32_tied1, svint32_t, svint16_t,
	     z0 = svaddwb_s32 (z0, z4),
	     z0 = svaddwb (z0, z4))

/*
** addwb_s32_tied2:
**	saddwb	z0\.s, z4\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (addwb_s32_tied2, svint32_t, svint16_t,
		 z0_res = svaddwb_s32 (z4, z0),
		 z0_res = svaddwb (z4, z0))

/*
** addwb_s32_untied:
**	saddwb	z0\.s, z1\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (addwb_s32_untied, svint32_t, svint16_t,
	     z0 = svaddwb_s32 (z1, z4),
	     z0 = svaddwb (z1, z4))

/*
** addwb_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	saddwb	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (addwb_w0_s32_tied1, svint32_t, int16_t,
		 z0 = svaddwb_n_s32 (z0, x0),
		 z0 = svaddwb (z0, x0))

/*
** addwb_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	saddwb	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (addwb_w0_s32_untied, svint32_t, int16_t,
		 z0 = svaddwb_n_s32 (z1, x0),
		 z0 = svaddwb (z1, x0))

/*
** addwb_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	saddwb	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (addwb_11_s32_tied1, svint32_t,
		z0 = svaddwb_n_s32 (z0, 11),
		z0 = svaddwb (z0, 11))

/*
** addwb_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	saddwb	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (addwb_11_s32_untied, svint32_t,
		z0 = svaddwb_n_s32 (z1, 11),
		z0 = svaddwb (z1, 11))
