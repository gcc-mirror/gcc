/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lasta_d0_bf16_tied:
**	lasta	h0, p0, z0\.h
**	ret
*/
TEST_REDUCTION_D (lasta_d0_bf16_tied, bfloat16_t, svbfloat16_t,
		  d0 = svlasta_bf16 (p0, z0),
		  d0 = svlasta (p0, z0))

/*
** lasta_d0_bf16_untied:
**	lasta	h0, p0, z1\.h
**	ret
*/
TEST_REDUCTION_D (lasta_d0_bf16_untied, bfloat16_t, svbfloat16_t,
		  d0 = svlasta_bf16 (p0, z1),
		  d0 = svlasta (p0, z1))
