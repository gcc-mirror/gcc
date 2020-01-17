/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** pmullt_u64_tied1:
**	pmullt	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullt_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svpmullt_u64 (z0, z1),
		    z0_res = svpmullt (z0, z1))

/*
** pmullt_u64_tied2:
**	pmullt	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullt_u64_tied2, svuint64_t, svuint32_t,
		    z0_res = svpmullt_u64 (z1, z0),
		    z0_res = svpmullt (z1, z0))

/*
** pmullt_u64_untied:
**	pmullt	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullt_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svpmullt_u64 (z1, z2),
		    z0_res = svpmullt (z1, z2))

/*
** pmullt_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	pmullt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (pmullt_w0_u64_tied1, svuint64_t, svuint32_t, uint32_t,
		     z0_res = svpmullt_n_u64 (z0, x0),
		     z0_res = svpmullt (z0, x0))

/*
** pmullt_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	pmullt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (pmullt_w0_u64_untied, svuint64_t, svuint32_t, uint32_t,
		     z0_res = svpmullt_n_u64 (z1, x0),
		     z0_res = svpmullt (z1, x0))

/*
** pmullt_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	pmullt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullt_11_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svpmullt_n_u64 (z0, 11),
		    z0_res = svpmullt (z0, 11))

/*
** pmullt_11_u64_untied:
**	mov	(z[0-9]+\.s), #11
**	pmullt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullt_11_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svpmullt_n_u64 (z1, 11),
		    z0_res = svpmullt (z1, 11))
