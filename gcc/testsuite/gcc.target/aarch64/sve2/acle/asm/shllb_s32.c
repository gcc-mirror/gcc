/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shllb_0_s32_tied1:
**	sshllb	z0\.s, z0\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_0_s32_tied1, svint32_t, svint16_t,
		    z0_res = svshllb_n_s32 (z0, 0),
		    z0_res = svshllb (z0, 0))

/*
** shllb_0_s32_untied:
**	sshllb	z0\.s, z1\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_0_s32_untied, svint32_t, svint16_t,
		    z0_res = svshllb_n_s32 (z1, 0),
		    z0_res = svshllb (z1, 0))

/*
** shllb_1_s32_tied1:
**	sshllb	z0\.s, z0\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_1_s32_tied1, svint32_t, svint16_t,
		    z0_res = svshllb_n_s32 (z0, 1),
		    z0_res = svshllb (z0, 1))

/*
** shllb_1_s32_untied:
**	sshllb	z0\.s, z1\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_1_s32_untied, svint32_t, svint16_t,
		    z0_res = svshllb_n_s32 (z1, 1),
		    z0_res = svshllb (z1, 1))

/*
** shllb_15_s32_tied1:
**	sshllb	z0\.s, z0\.h, #15
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_15_s32_tied1, svint32_t, svint16_t,
		    z0_res = svshllb_n_s32 (z0, 15),
		    z0_res = svshllb (z0, 15))

/*
** shllb_15_s32_untied:
**	sshllb	z0\.s, z1\.h, #15
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_15_s32_untied, svint32_t, svint16_t,
		    z0_res = svshllb_n_s32 (z1, 15),
		    z0_res = svshllb (z1, 15))
