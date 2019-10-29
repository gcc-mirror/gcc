/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** maxv_x0_s8:
**	smaxv	b([0-9]+), p0, z0\.b
**	umov	w0, v\1\.b\[0\]
**	ret
*/
TEST_REDUCTION_X (maxv_x0_s8, int8_t, svint8_t,
		  x0 = svmaxv_s8 (p0, z0),
		  x0 = svmaxv (p0, z0))
