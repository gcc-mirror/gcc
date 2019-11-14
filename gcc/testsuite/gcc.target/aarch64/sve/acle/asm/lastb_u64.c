/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lastb_x0_u64:
**	lastb	x0, p0, z0\.d
**	ret
*/
TEST_REDUCTION_X (lastb_x0_u64, uint64_t, svuint64_t,
		  x0 = svlastb_u64 (p0, z0),
		  x0 = svlastb (p0, z0))
