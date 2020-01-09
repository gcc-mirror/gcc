/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** movlt_s64_tied1:
**	sshllt	z0\.d, z0\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlt_s64_tied1, svint64_t, svint32_t,
		    z0_res = svmovlt_s64 (z0),
		    z0_res = svmovlt (z0))

/*
** movlt_s64_untied:
**	sshllt	z0\.d, z1\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlt_s64_untied, svint64_t, svint32_t,
		    z0_res = svmovlt_s64 (z1),
		    z0_res = svmovlt (z1))
