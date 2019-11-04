/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lasta_d0_f32_tied:
**	lasta	s0, p0, z0\.s
**	ret
*/
TEST_REDUCTION_D (lasta_d0_f32_tied, float32_t, svfloat32_t,
		  d0 = svlasta_f32 (p0, z0),
		  d0 = svlasta (p0, z0))

/*
** lasta_d0_f32_untied:
**	lasta	s0, p0, z1\.s
**	ret
*/
TEST_REDUCTION_D (lasta_d0_f32_untied, float32_t, svfloat32_t,
		  d0 = svlasta_f32 (p0, z1),
		  d0 = svlasta (p0, z1))
