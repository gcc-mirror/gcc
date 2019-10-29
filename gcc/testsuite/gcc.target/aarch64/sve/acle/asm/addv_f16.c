/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addv_d0_f16_tied:
**	faddv	h0, p0, z0\.h
**	ret
*/
TEST_REDUCTION_D (addv_d0_f16_tied, float16_t, svfloat16_t,
		  d0 = svaddv_f16 (p0, z0),
		  d0 = svaddv (p0, z0))

/*
** addv_d0_f16_untied:
**	faddv	h0, p0, z1\.h
**	ret
*/
TEST_REDUCTION_D (addv_d0_f16_untied, float16_t, svfloat16_t,
		  d0 = svaddv_f16 (p0, z1),
		  d0 = svaddv (p0, z1))
