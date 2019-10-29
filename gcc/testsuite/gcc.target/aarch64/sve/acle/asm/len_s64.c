/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** len_x0_s64:
**	cntd	x0
**	ret
*/
TEST_REDUCTION_X (len_x0_s64, uint64_t, svint64_t,
		  x0 = svlen_s64 (z0),
		  x0 = svlen (z0))
