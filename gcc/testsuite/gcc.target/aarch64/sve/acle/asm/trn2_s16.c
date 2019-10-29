/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn2_s16_tied1:
**	trn2	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (trn2_s16_tied1, svint16_t,
		z0 = svtrn2_s16 (z0, z1),
		z0 = svtrn2 (z0, z1))

/*
** trn2_s16_tied2:
**	trn2	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (trn2_s16_tied2, svint16_t,
		z0 = svtrn2_s16 (z1, z0),
		z0 = svtrn2 (z1, z0))

/*
** trn2_s16_untied:
**	trn2	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (trn2_s16_untied, svint16_t,
		z0 = svtrn2_s16 (z1, z2),
		z0 = svtrn2 (z1, z2))
