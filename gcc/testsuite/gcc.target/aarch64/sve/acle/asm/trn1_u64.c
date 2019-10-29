/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn1_u64_tied1:
**	trn1	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (trn1_u64_tied1, svuint64_t,
		z0 = svtrn1_u64 (z0, z1),
		z0 = svtrn1 (z0, z1))

/*
** trn1_u64_tied2:
**	trn1	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (trn1_u64_tied2, svuint64_t,
		z0 = svtrn1_u64 (z1, z0),
		z0 = svtrn1 (z1, z0))

/*
** trn1_u64_untied:
**	trn1	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (trn1_u64_untied, svuint64_t,
		z0 = svtrn1_u64 (z1, z2),
		z0 = svtrn1 (z1, z2))
