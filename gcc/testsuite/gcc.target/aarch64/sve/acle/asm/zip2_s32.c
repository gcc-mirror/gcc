/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip2_s32_tied1:
**	zip2	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (zip2_s32_tied1, svint32_t,
		z0 = svzip2_s32 (z0, z1),
		z0 = svzip2 (z0, z1))

/*
** zip2_s32_tied2:
**	zip2	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (zip2_s32_tied2, svint32_t,
		z0 = svzip2_s32 (z1, z0),
		z0 = svzip2 (z1, z0))

/*
** zip2_s32_untied:
**	zip2	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (zip2_s32_untied, svint32_t,
		z0 = svzip2_s32 (z1, z2),
		z0 = svzip2 (z1, z2))
