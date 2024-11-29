/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** len_x0_mf8:
**	cntb	x0
**	ret
*/
TEST_REDUCTION_X (len_x0_mf8, uint64_t, svmfloat8_t,
		  x0 = svlen_mf8 (z0),
		  x0 = svlen (z0))
