/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shllb_0_s64_tied1:
**	sshllb	z0\.d, z0\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_0_s64_tied1, svint64_t, svint32_t,
		    z0_res = svshllb_n_s64 (z0, 0),
		    z0_res = svshllb (z0, 0))

/*
** shllb_0_s64_untied:
**	sshllb	z0\.d, z1\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_0_s64_untied, svint64_t, svint32_t,
		    z0_res = svshllb_n_s64 (z1, 0),
		    z0_res = svshllb (z1, 0))

/*
** shllb_1_s64_tied1:
**	sshllb	z0\.d, z0\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_1_s64_tied1, svint64_t, svint32_t,
		    z0_res = svshllb_n_s64 (z0, 1),
		    z0_res = svshllb (z0, 1))

/*
** shllb_1_s64_untied:
**	sshllb	z0\.d, z1\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_1_s64_untied, svint64_t, svint32_t,
		    z0_res = svshllb_n_s64 (z1, 1),
		    z0_res = svshllb (z1, 1))

/*
** shllb_31_s64_tied1:
**	sshllb	z0\.d, z0\.s, #31
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_31_s64_tied1, svint64_t, svint32_t,
		    z0_res = svshllb_n_s64 (z0, 31),
		    z0_res = svshllb (z0, 31))

/*
** shllb_31_s64_untied:
**	sshllb	z0\.d, z1\.s, #31
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_31_s64_untied, svint64_t, svint32_t,
		    z0_res = svshllb_n_s64 (z1, 31),
		    z0_res = svshllb (z1, 31))
