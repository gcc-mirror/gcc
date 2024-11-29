/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lastb_x0_mf8:
**	lastb	b0, p0, z0\.b
**	ret
*/
TEST_REDUCTION_X (lastb_x0_mf8, mfloat8_t, svmfloat8_t,
		  x0 = svlastb_mf8 (p0, z0),
		  x0 = svlastb (p0, z0))
