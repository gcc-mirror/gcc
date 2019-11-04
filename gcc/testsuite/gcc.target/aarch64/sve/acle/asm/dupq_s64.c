/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dupq_pool_s64:
**	...
**	ld1rqd	z0\.d, p[0-7]/z, \[x[0-9]+\]
**	ret
*/
TEST_UNIFORM_Z (dupq_pool_s64, svint64_t,
		z0 = svdupq_n_s64 (4, 10),
		z0 = svdupq_s64 (4, 10))
