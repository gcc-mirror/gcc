/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subwb_s32_tied1:
**	ssubwb	z0\.s, z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (subwb_s32_tied1, svint32_t, svint16_t,
	     z0 = svsubwb_s32 (z0, z4),
	     z0 = svsubwb (z0, z4))

/*
** subwb_s32_tied2:
**	ssubwb	z0\.s, z4\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (subwb_s32_tied2, svint32_t, svint16_t,
		 z0_res = svsubwb_s32 (z4, z0),
		 z0_res = svsubwb (z4, z0))

/*
** subwb_s32_untied:
**	ssubwb	z0\.s, z1\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (subwb_s32_untied, svint32_t, svint16_t,
	     z0 = svsubwb_s32 (z1, z4),
	     z0 = svsubwb (z1, z4))

/*
** subwb_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	ssubwb	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (subwb_w0_s32_tied1, svint32_t, int16_t,
		 z0 = svsubwb_n_s32 (z0, x0),
		 z0 = svsubwb (z0, x0))

/*
** subwb_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	ssubwb	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (subwb_w0_s32_untied, svint32_t, int16_t,
		 z0 = svsubwb_n_s32 (z1, x0),
		 z0 = svsubwb (z1, x0))

/*
** subwb_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	ssubwb	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (subwb_11_s32_tied1, svint32_t,
		z0 = svsubwb_n_s32 (z0, 11),
		z0 = svsubwb (z0, 11))

/*
** subwb_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	ssubwb	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (subwb_11_s32_untied, svint32_t,
		z0 = svsubwb_n_s32 (z1, 11),
		z0 = svsubwb (z1, 11))
