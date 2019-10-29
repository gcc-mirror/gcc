/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addv_x0_s16:
**	saddv	(d[0-9]+), p0, z0\.h
**	fmov	x0, \1
**	ret
*/
TEST_REDUCTION_X (addv_x0_s16, int64_t, svint16_t,
		  x0 = svaddv_s16 (p0, z0),
		  x0 = svaddv (p0, z0))
