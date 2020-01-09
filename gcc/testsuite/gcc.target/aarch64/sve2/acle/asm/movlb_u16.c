/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** movlb_u16_tied1:
**	ushllb	z0\.h, z0\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlb_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svmovlb_u16 (z0),
		    z0_res = svmovlb (z0))

/*
** movlb_u16_untied:
**	ushllb	z0\.h, z1\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlb_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svmovlb_u16 (z1),
		    z0_res = svmovlb (z1))
