/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** len_x0_s16:
**	cnth	x0
**	ret
*/
TEST_REDUCTION_X (len_x0_s16, uint64_t, svint16_t,
		  x0 = svlen_s16 (z0),
		  x0 = svlen (z0))
