/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dupq_pool_f64:
**	...
**	ld1rqd	z0\.d, p[0-7]/z, \[x[0-9]+\]
**	ret
*/
TEST_UNIFORM_Z (dupq_pool_f64, svfloat64_t,
		z0 = svdupq_n_f64 (4.5, 10.1),
		z0 = svdupq_f64 (4.5, 10.1))
