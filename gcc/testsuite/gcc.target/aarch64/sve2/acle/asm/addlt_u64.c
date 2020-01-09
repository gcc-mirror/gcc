/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addlt_u64_tied1:
**	uaddlt	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svaddlt_u64 (z0, z1),
		    z0_res = svaddlt (z0, z1))

/*
** addlt_u64_tied2:
**	uaddlt	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_u64_tied2, svuint64_t, svuint32_t,
		    z0_res = svaddlt_u64 (z1, z0),
		    z0_res = svaddlt (z1, z0))

/*
** addlt_u64_untied:
**	uaddlt	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svaddlt_u64 (z1, z2),
		    z0_res = svaddlt (z1, z2))

/*
** addlt_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	uaddlt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlt_w0_u64_tied1, svuint64_t, svuint32_t, uint32_t,
		     z0_res = svaddlt_n_u64 (z0, x0),
		     z0_res = svaddlt (z0, x0))

/*
** addlt_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	uaddlt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlt_w0_u64_untied, svuint64_t, svuint32_t, uint32_t,
		     z0_res = svaddlt_n_u64 (z1, x0),
		     z0_res = svaddlt (z1, x0))

/*
** addlt_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	uaddlt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_11_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svaddlt_n_u64 (z0, 11),
		    z0_res = svaddlt (z0, 11))

/*
** addlt_11_u64_untied:
**	mov	(z[0-9]+\.s), #11
**	uaddlt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_11_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svaddlt_n_u64 (z1, 11),
		    z0_res = svaddlt (z1, 11))
