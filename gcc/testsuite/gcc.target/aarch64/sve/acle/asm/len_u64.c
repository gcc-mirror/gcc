/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** len_x0_u64:
**	cntd	x0
**	ret
*/
TEST_REDUCTION_X (len_x0_u64, uint64_t, svuint64_t,
		  x0 = svlen_u64 (z0),
		  x0 = svlen (z0))
