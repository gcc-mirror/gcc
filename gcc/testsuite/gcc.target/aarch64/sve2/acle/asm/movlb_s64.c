/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** movlb_s64_tied1:
**	sshllb	z0\.d, z0\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlb_s64_tied1, svint64_t, svint32_t,
		    z0_res = svmovlb_s64 (z0),
		    z0_res = svmovlb (z0))

/*
** movlb_s64_untied:
**	sshllb	z0\.d, z1\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlb_s64_untied, svint64_t, svint32_t,
		    z0_res = svmovlb_s64 (z1),
		    z0_res = svmovlb (z1))
