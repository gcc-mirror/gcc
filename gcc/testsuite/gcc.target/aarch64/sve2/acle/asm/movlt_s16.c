/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** movlt_s16_tied1:
**	sshllt	z0\.h, z0\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlt_s16_tied1, svint16_t, svint8_t,
		    z0_res = svmovlt_s16 (z0),
		    z0_res = svmovlt (z0))

/*
** movlt_s16_untied:
**	sshllt	z0\.h, z1\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (movlt_s16_untied, svint16_t, svint8_t,
		    z0_res = svmovlt_s16 (z1),
		    z0_res = svmovlt (z1))
