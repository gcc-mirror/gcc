/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn1_f16_tied1:
**	trn1	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (trn1_f16_tied1, svfloat16_t,
		z0 = svtrn1_f16 (z0, z1),
		z0 = svtrn1 (z0, z1))

/*
** trn1_f16_tied2:
**	trn1	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (trn1_f16_tied2, svfloat16_t,
		z0 = svtrn1_f16 (z1, z0),
		z0 = svtrn1 (z1, z0))

/*
** trn1_f16_untied:
**	trn1	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (trn1_f16_untied, svfloat16_t,
		z0 = svtrn1_f16 (z1, z2),
		z0 = svtrn1 (z1, z2))
