/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lastb_d0_bf16_tied:
**	lastb	h0, p0, z0\.h
**	ret
*/
TEST_REDUCTION_D (lastb_d0_bf16_tied, bfloat16_t, svbfloat16_t,
		  d0 = svlastb_bf16 (p0, z0),
		  d0 = svlastb (p0, z0))

/*
** lastb_d0_bf16_untied:
**	lastb	h0, p0, z1\.h
**	ret
*/
TEST_REDUCTION_D (lastb_d0_bf16_untied, bfloat16_t, svbfloat16_t,
		  d0 = svlastb_bf16 (p0, z1),
		  d0 = svlastb (p0, z1))
