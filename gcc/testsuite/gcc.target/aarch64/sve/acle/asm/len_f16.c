/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** len_x0_f16:
**	cnth	x0
**	ret
*/
TEST_REDUCTION_X (len_x0_f16, uint64_t, svfloat16_t,
		  x0 = svlen_f16 (z0),
		  x0 = svlen (z0))
