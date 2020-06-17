/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** movlb_u32_tied1:
**	ushllb	z0\.s, z0\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlb_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svmovlb_u32 (z0),
		    z0_res = svmovlb (z0))

/*
** movlb_u32_untied:
**	ushllb	z0\.s, z1\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlb_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svmovlb_u32 (z1),
		    z0_res = svmovlb (z1))
