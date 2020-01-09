/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** movlb_s16_tied1:
**	sshllb	z0\.h, z0\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlb_s16_tied1, svint16_t, svint8_t,
		    z0_res = svmovlb_s16 (z0),
		    z0_res = svmovlb (z0))

/*
** movlb_s16_untied:
**	sshllb	z0\.h, z1\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlb_s16_untied, svint16_t, svint8_t,
		    z0_res = svmovlb_s16 (z1),
		    z0_res = svmovlb (z1))
