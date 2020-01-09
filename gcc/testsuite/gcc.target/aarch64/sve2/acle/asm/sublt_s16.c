/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sublt_s16_tied1:
**	ssublt	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_s16_tied1, svint16_t, svint8_t,
		    z0_res = svsublt_s16 (z0, z1),
		    z0_res = svsublt (z0, z1))

/*
** sublt_s16_tied2:
**	ssublt	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_s16_tied2, svint16_t, svint8_t,
		    z0_res = svsublt_s16 (z1, z0),
		    z0_res = svsublt (z1, z0))

/*
** sublt_s16_untied:
**	ssublt	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_s16_untied, svint16_t, svint8_t,
		    z0_res = svsublt_s16 (z1, z2),
		    z0_res = svsublt (z1, z2))

/*
** sublt_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	ssublt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublt_w0_s16_tied1, svint16_t, svint8_t, int8_t,
		     z0_res = svsublt_n_s16 (z0, x0),
		     z0_res = svsublt (z0, x0))

/*
** sublt_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	ssublt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublt_w0_s16_untied, svint16_t, svint8_t, int8_t,
		     z0_res = svsublt_n_s16 (z1, x0),
		     z0_res = svsublt (z1, x0))

/*
** sublt_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	ssublt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_11_s16_tied1, svint16_t, svint8_t,
		    z0_res = svsublt_n_s16 (z0, 11),
		    z0_res = svsublt (z0, 11))

/*
** sublt_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	ssublt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_11_s16_untied, svint16_t, svint8_t,
		    z0_res = svsublt_n_s16 (z1, 11),
		    z0_res = svsublt (z1, 11))
