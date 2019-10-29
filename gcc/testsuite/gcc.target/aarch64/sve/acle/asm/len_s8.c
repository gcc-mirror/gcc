/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** len_x0_s8:
**	cntb	x0
**	ret
*/
TEST_REDUCTION_X (len_x0_s8, uint64_t, svint8_t,
		  x0 = svlen_s8 (z0),
		  x0 = svlen (z0))
