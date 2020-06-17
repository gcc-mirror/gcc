/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shllt_0_s32_tied1:
**	sshllt	z0\.s, z0\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_0_s32_tied1, svint32_t, svint16_t,
		    z0_res = svshllt_n_s32 (z0, 0),
		    z0_res = svshllt (z0, 0))

/*
** shllt_0_s32_untied:
**	sshllt	z0\.s, z1\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_0_s32_untied, svint32_t, svint16_t,
		    z0_res = svshllt_n_s32 (z1, 0),
		    z0_res = svshllt (z1, 0))

/*
** shllt_1_s32_tied1:
**	sshllt	z0\.s, z0\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_1_s32_tied1, svint32_t, svint16_t,
		    z0_res = svshllt_n_s32 (z0, 1),
		    z0_res = svshllt (z0, 1))

/*
** shllt_1_s32_untied:
**	sshllt	z0\.s, z1\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_1_s32_untied, svint32_t, svint16_t,
		    z0_res = svshllt_n_s32 (z1, 1),
		    z0_res = svshllt (z1, 1))

/*
** shllt_15_s32_tied1:
**	sshllt	z0\.s, z0\.h, #15
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_15_s32_tied1, svint32_t, svint16_t,
		    z0_res = svshllt_n_s32 (z0, 15),
		    z0_res = svshllt (z0, 15))

/*
** shllt_15_s32_untied:
**	sshllt	z0\.s, z1\.h, #15
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_15_s32_untied, svint32_t, svint16_t,
		    z0_res = svshllt_n_s32 (z1, 15),
		    z0_res = svshllt (z1, 15))
