/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lastb_x0_s8:
**	lastb	w0, p0, z0\.b
**	ret
*/
TEST_REDUCTION_X (lastb_x0_s8, int8_t, svint8_t,
		  x0 = svlastb_s8 (p0, z0),
		  x0 = svlastb (p0, z0))
