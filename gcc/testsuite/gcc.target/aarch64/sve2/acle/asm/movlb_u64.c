/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** movlb_u64_tied1:
**	ushllb	z0\.d, z0\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlb_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svmovlb_u64 (z0),
		    z0_res = svmovlb (z0))

/*
** movlb_u64_untied:
**	ushllb	z0\.d, z1\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlb_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svmovlb_u64 (z1),
		    z0_res = svmovlb (z1))
