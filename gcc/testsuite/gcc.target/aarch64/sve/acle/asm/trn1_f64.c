/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn1_f64_tied1:
**	trn1	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (trn1_f64_tied1, svfloat64_t,
		z0 = svtrn1_f64 (z0, z1),
		z0 = svtrn1 (z0, z1))

/*
** trn1_f64_tied2:
**	trn1	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (trn1_f64_tied2, svfloat64_t,
		z0 = svtrn1_f64 (z1, z0),
		z0 = svtrn1 (z1, z0))

/*
** trn1_f64_untied:
**	trn1	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (trn1_f64_untied, svfloat64_t,
		z0 = svtrn1_f64 (z1, z2),
		z0 = svtrn1 (z1, z2))
