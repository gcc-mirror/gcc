/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn1_s8_tied1:
**	trn1	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (trn1_s8_tied1, svint8_t,
		z0 = svtrn1_s8 (z0, z1),
		z0 = svtrn1 (z0, z1))

/*
** trn1_s8_tied2:
**	trn1	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (trn1_s8_tied2, svint8_t,
		z0 = svtrn1_s8 (z1, z0),
		z0 = svtrn1 (z1, z0))

/*
** trn1_s8_untied:
**	trn1	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (trn1_s8_untied, svint8_t,
		z0 = svtrn1_s8 (z1, z2),
		z0 = svtrn1 (z1, z2))
