/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** movlt_s32_tied1:
**	sshllt	z0\.s, z0\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlt_s32_tied1, svint32_t, svint16_t,
		    z0_res = svmovlt_s32 (z0),
		    z0_res = svmovlt (z0))

/*
** movlt_s32_untied:
**	sshllt	z0\.s, z1\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlt_s32_untied, svint32_t, svint16_t,
		    z0_res = svmovlt_s32 (z1),
		    z0_res = svmovlt (z1))
