/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** movlb_s32_tied1:
**	sshllb	z0\.s, z0\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlb_s32_tied1, svint32_t, svint16_t,
		    z0_res = svmovlb_s32 (z0),
		    z0_res = svmovlb (z0))

/*
** movlb_s32_untied:
**	sshllb	z0\.s, z1\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlb_s32_untied, svint32_t, svint16_t,
		    z0_res = svmovlb_s32 (z1),
		    z0_res = svmovlb (z1))
