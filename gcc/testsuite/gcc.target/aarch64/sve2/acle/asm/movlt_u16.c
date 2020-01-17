/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** movlt_u16_tied1:
**	ushllt	z0\.h, z0\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlt_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svmovlt_u16 (z0),
		    z0_res = svmovlt (z0))

/*
** movlt_u16_untied:
**	ushllt	z0\.h, z1\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlt_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svmovlt_u16 (z1),
		    z0_res = svmovlt (z1))
