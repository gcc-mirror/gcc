/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shllt_0_u64_tied1:
**	ushllt	z0\.d, z0\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_0_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svshllt_n_u64 (z0, 0),
		    z0_res = svshllt (z0, 0))

/*
** shllt_0_u64_untied:
**	ushllt	z0\.d, z1\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_0_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svshllt_n_u64 (z1, 0),
		    z0_res = svshllt (z1, 0))

/*
** shllt_1_u64_tied1:
**	ushllt	z0\.d, z0\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_1_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svshllt_n_u64 (z0, 1),
		    z0_res = svshllt (z0, 1))

/*
** shllt_1_u64_untied:
**	ushllt	z0\.d, z1\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_1_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svshllt_n_u64 (z1, 1),
		    z0_res = svshllt (z1, 1))

/*
** shllt_31_u64_tied1:
**	ushllt	z0\.d, z0\.s, #31
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_31_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svshllt_n_u64 (z0, 31),
		    z0_res = svshllt (z0, 31))

/*
** shllt_31_u64_untied:
**	ushllt	z0\.d, z1\.s, #31
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_31_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svshllt_n_u64 (z1, 31),
		    z0_res = svshllt (z1, 31))
