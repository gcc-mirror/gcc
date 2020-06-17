/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shllb_0_s16_tied1:
**	sshllb	z0\.h, z0\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_0_s16_tied1, svint16_t, svint8_t,
		    z0_res = svshllb_n_s16 (z0, 0),
		    z0_res = svshllb (z0, 0))

/*
** shllb_0_s16_untied:
**	sshllb	z0\.h, z1\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_0_s16_untied, svint16_t, svint8_t,
		    z0_res = svshllb_n_s16 (z1, 0),
		    z0_res = svshllb (z1, 0))

/*
** shllb_1_s16_tied1:
**	sshllb	z0\.h, z0\.b, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_1_s16_tied1, svint16_t, svint8_t,
		    z0_res = svshllb_n_s16 (z0, 1),
		    z0_res = svshllb (z0, 1))

/*
** shllb_1_s16_untied:
**	sshllb	z0\.h, z1\.b, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_1_s16_untied, svint16_t, svint8_t,
		    z0_res = svshllb_n_s16 (z1, 1),
		    z0_res = svshllb (z1, 1))

/*
** shllb_7_s16_tied1:
**	sshllb	z0\.h, z0\.b, #7
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_7_s16_tied1, svint16_t, svint8_t,
		    z0_res = svshllb_n_s16 (z0, 7),
		    z0_res = svshllb (z0, 7))

/*
** shllb_7_s16_untied:
**	sshllb	z0\.h, z1\.b, #7
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_7_s16_untied, svint16_t, svint8_t,
		    z0_res = svshllb_n_s16 (z1, 7),
		    z0_res = svshllb (z1, 7))
