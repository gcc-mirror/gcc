/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subltb_s32_tied1:
**	ssubltb	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_s32_tied1, svint32_t, svint16_t,
		    z0_res = svsubltb_s32 (z0, z1),
		    z0_res = svsubltb (z0, z1))

/*
** subltb_s32_tied2:
**	ssubltb	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_s32_tied2, svint32_t, svint16_t,
		    z0_res = svsubltb_s32 (z1, z0),
		    z0_res = svsubltb (z1, z0))

/*
** subltb_s32_untied:
**	ssubltb	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_s32_untied, svint32_t, svint16_t,
		    z0_res = svsubltb_s32 (z1, z2),
		    z0_res = svsubltb (z1, z2))

/*
** subltb_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	ssubltb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (subltb_w0_s32_tied1, svint32_t, svint16_t, int16_t,
		     z0_res = svsubltb_n_s32 (z0, x0),
		     z0_res = svsubltb (z0, x0))

/*
** subltb_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	ssubltb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (subltb_w0_s32_untied, svint32_t, svint16_t, int16_t,
		     z0_res = svsubltb_n_s32 (z1, x0),
		     z0_res = svsubltb (z1, x0))

/*
** subltb_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	ssubltb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_11_s32_tied1, svint32_t, svint16_t,
		    z0_res = svsubltb_n_s32 (z0, 11),
		    z0_res = svsubltb (z0, 11))

/*
** subltb_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	ssubltb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_11_s32_untied, svint32_t, svint16_t,
		    z0_res = svsubltb_n_s32 (z1, 11),
		    z0_res = svsubltb (z1, 11))
