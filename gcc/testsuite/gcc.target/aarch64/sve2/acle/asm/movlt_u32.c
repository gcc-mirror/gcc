/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** movlt_u32_tied1:
**	ushllt	z0\.s, z0\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlt_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svmovlt_u32 (z0),
		    z0_res = svmovlt (z0))

/*
** movlt_u32_untied:
**	ushllt	z0\.s, z1\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlt_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svmovlt_u32 (z1),
		    z0_res = svmovlt (z1))
