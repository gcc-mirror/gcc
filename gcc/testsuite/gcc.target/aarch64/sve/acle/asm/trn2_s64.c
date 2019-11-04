/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn2_s64_tied1:
**	trn2	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (trn2_s64_tied1, svint64_t,
		z0 = svtrn2_s64 (z0, z1),
		z0 = svtrn2 (z0, z1))

/*
** trn2_s64_tied2:
**	trn2	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (trn2_s64_tied2, svint64_t,
		z0 = svtrn2_s64 (z1, z0),
		z0 = svtrn2 (z1, z0))

/*
** trn2_s64_untied:
**	trn2	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (trn2_s64_untied, svint64_t,
		z0 = svtrn2_s64 (z1, z2),
		z0 = svtrn2 (z1, z2))
