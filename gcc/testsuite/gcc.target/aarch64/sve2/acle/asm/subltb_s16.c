/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subltb_s16_tied1:
**	ssubltb	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_s16_tied1, svint16_t, svint8_t,
		    z0_res = svsubltb_s16 (z0, z1),
		    z0_res = svsubltb (z0, z1))

/*
** subltb_s16_tied2:
**	ssubltb	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_s16_tied2, svint16_t, svint8_t,
		    z0_res = svsubltb_s16 (z1, z0),
		    z0_res = svsubltb (z1, z0))

/*
** subltb_s16_untied:
**	ssubltb	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_s16_untied, svint16_t, svint8_t,
		    z0_res = svsubltb_s16 (z1, z2),
		    z0_res = svsubltb (z1, z2))

/*
** subltb_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	ssubltb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (subltb_w0_s16_tied1, svint16_t, svint8_t, int8_t,
		     z0_res = svsubltb_n_s16 (z0, x0),
		     z0_res = svsubltb (z0, x0))

/*
** subltb_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	ssubltb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (subltb_w0_s16_untied, svint16_t, svint8_t, int8_t,
		     z0_res = svsubltb_n_s16 (z1, x0),
		     z0_res = svsubltb (z1, x0))

/*
** subltb_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	ssubltb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_11_s16_tied1, svint16_t, svint8_t,
		    z0_res = svsubltb_n_s16 (z0, 11),
		    z0_res = svsubltb (z0, 11))

/*
** subltb_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	ssubltb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_11_s16_untied, svint16_t, svint8_t,
		    z0_res = svsubltb_n_s16 (z1, 11),
		    z0_res = svsubltb (z1, 11))
