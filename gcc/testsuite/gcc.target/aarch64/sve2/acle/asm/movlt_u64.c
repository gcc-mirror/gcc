/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** movlt_u64_tied1:
**	ushllt	z0\.d, z0\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlt_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svmovlt_u64 (z0),
		    z0_res = svmovlt (z0))

/*
** movlt_u64_untied:
**	ushllt	z0\.d, z1\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlt_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svmovlt_u64 (z1),
		    z0_res = svmovlt (z1))
