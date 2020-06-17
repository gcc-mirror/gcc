/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sublt_s64_tied1:
**	ssublt	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_s64_tied1, svint64_t, svint32_t,
		    z0_res = svsublt_s64 (z0, z1),
		    z0_res = svsublt (z0, z1))

/*
** sublt_s64_tied2:
**	ssublt	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_s64_tied2, svint64_t, svint32_t,
		    z0_res = svsublt_s64 (z1, z0),
		    z0_res = svsublt (z1, z0))

/*
** sublt_s64_untied:
**	ssublt	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_s64_untied, svint64_t, svint32_t,
		    z0_res = svsublt_s64 (z1, z2),
		    z0_res = svsublt (z1, z2))

/*
** sublt_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	ssublt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublt_w0_s64_tied1, svint64_t, svint32_t, int32_t,
		     z0_res = svsublt_n_s64 (z0, x0),
		     z0_res = svsublt (z0, x0))

/*
** sublt_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	ssublt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublt_w0_s64_untied, svint64_t, svint32_t, int32_t,
		     z0_res = svsublt_n_s64 (z1, x0),
		     z0_res = svsublt (z1, x0))

/*
** sublt_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	ssublt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_11_s64_tied1, svint64_t, svint32_t,
		    z0_res = svsublt_n_s64 (z0, 11),
		    z0_res = svsublt (z0, 11))

/*
** sublt_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	ssublt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublt_11_s64_untied, svint64_t, svint32_t,
		    z0_res = svsublt_n_s64 (z1, 11),
		    z0_res = svsublt (z1, 11))
