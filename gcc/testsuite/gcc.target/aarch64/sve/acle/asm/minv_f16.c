/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** minv_d0_f16_tied:
**	fminv	h0, p0, z0\.h
**	ret
*/
TEST_REDUCTION_D (minv_d0_f16_tied, float16_t, svfloat16_t,
		  d0 = svminv_f16 (p0, z0),
		  d0 = svminv (p0, z0))

/*
** minv_d0_f16_untied:
**	fminv	h0, p0, z1\.h
**	ret
*/
TEST_REDUCTION_D (minv_d0_f16_untied, float16_t, svfloat16_t,
		  d0 = svminv_f16 (p0, z1),
		  d0 = svminv (p0, z1))
