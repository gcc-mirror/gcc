/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** len_x0_f32:
**	cntw	x0
**	ret
*/
TEST_REDUCTION_X (len_x0_f32, uint64_t, svfloat32_t,
		  x0 = svlen_f32 (z0),
		  x0 = svlen (z0))
