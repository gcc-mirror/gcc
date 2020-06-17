/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subltb_s64_tied1:
**	ssubltb	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_s64_tied1, svint64_t, svint32_t,
		    z0_res = svsubltb_s64 (z0, z1),
		    z0_res = svsubltb (z0, z1))

/*
** subltb_s64_tied2:
**	ssubltb	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_s64_tied2, svint64_t, svint32_t,
		    z0_res = svsubltb_s64 (z1, z0),
		    z0_res = svsubltb (z1, z0))

/*
** subltb_s64_untied:
**	ssubltb	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_s64_untied, svint64_t, svint32_t,
		    z0_res = svsubltb_s64 (z1, z2),
		    z0_res = svsubltb (z1, z2))

/*
** subltb_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	ssubltb	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (subltb_w0_s64_tied1, svint64_t, svint32_t, int32_t,
		     z0_res = svsubltb_n_s64 (z0, x0),
		     z0_res = svsubltb (z0, x0))

/*
** subltb_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	ssubltb	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (subltb_w0_s64_untied, svint64_t, svint32_t, int32_t,
		     z0_res = svsubltb_n_s64 (z1, x0),
		     z0_res = svsubltb (z1, x0))

/*
** subltb_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	ssubltb	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_11_s64_tied1, svint64_t, svint32_t,
		    z0_res = svsubltb_n_s64 (z0, 11),
		    z0_res = svsubltb (z0, 11))

/*
** subltb_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	ssubltb	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (subltb_11_s64_untied, svint64_t, svint32_t,
		    z0_res = svsubltb_n_s64 (z1, 11),
		    z0_res = svsubltb (z1, 11))
