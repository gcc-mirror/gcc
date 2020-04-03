/* { dg-require-effective-target aarch64_asm_f64mm_ok } */
/* { dg-additional-options "-march=armv8.2-a+f64mm" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip2q_f32_tied1:
**	zip2	z0\.q, z0\.q, z1\.q
**	ret
*/
TEST_UNIFORM_Z (zip2q_f32_tied1, svfloat32_t,
		z0 = svzip2q_f32 (z0, z1),
		z0 = svzip2q (z0, z1))

/*
** zip2q_f32_tied2:
**	zip2	z0\.q, z1\.q, z0\.q
**	ret
*/
TEST_UNIFORM_Z (zip2q_f32_tied2, svfloat32_t,
		z0 = svzip2q_f32 (z1, z0),
		z0 = svzip2q (z1, z0))

/*
** zip2q_f32_untied:
**	zip2	z0\.q, z1\.q, z2\.q
**	ret
*/
TEST_UNIFORM_Z (zip2q_f32_untied, svfloat32_t,
		z0 = svzip2q_f32 (z1, z2),
		z0 = svzip2q (z1, z2))
