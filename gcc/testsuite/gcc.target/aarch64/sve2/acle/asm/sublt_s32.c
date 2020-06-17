/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sublt_s32_tied1:
**	ssublt	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_s32_tied1, svint32_t, svint16_t,
		    z0_res = svsublt_s32 (z0, z1),
		    z0_res = svsublt (z0, z1))

/*
** sublt_s32_tied2:
**	ssublt	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_s32_tied2, svint32_t, svint16_t,
		    z0_res = svsublt_s32 (z1, z0),
		    z0_res = svsublt (z1, z0))

/*
** sublt_s32_untied:
**	ssublt	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_s32_untied, svint32_t, svint16_t,
		    z0_res = svsublt_s32 (z1, z2),
		    z0_res = svsublt (z1, z2))

/*
** sublt_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	ssublt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublt_w0_s32_tied1, svint32_t, svint16_t, int16_t,
		     z0_res = svsublt_n_s32 (z0, x0),
		     z0_res = svsublt (z0, x0))

/*
** sublt_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	ssublt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublt_w0_s32_untied, svint32_t, svint16_t, int16_t,
		     z0_res = svsublt_n_s32 (z1, x0),
		     z0_res = svsublt (z1, x0))

/*
** sublt_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	ssublt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_11_s32_tied1, svint32_t, svint16_t,
		    z0_res = svsublt_n_s32 (z0, 11),
		    z0_res = svsublt (z0, 11))

/*
** sublt_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	ssublt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_11_s32_untied, svint32_t, svint16_t,
		    z0_res = svsublt_n_s32 (z1, 11),
		    z0_res = svsublt (z1, 11))
