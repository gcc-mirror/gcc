/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** minv_x0_s64:
**	sminv	(d[0-9]+), p0, z0\.d
**	fmov	x0, \1
**	ret
*/
TEST_REDUCTION_X (minv_x0_s64, int64_t, svint64_t,
		  x0 = svminv_s64 (p0, z0),
		  x0 = svminv (p0, z0))
