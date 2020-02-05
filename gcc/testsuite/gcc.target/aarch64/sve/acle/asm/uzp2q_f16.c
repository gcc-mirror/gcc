/* { dg-require-effective-target aarch64_asm_f64mm_ok } */
/* { dg-additional-options "-march=armv8.2-a+f64mm" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uzp2q_f16_tied1:
**	uzp2	z0\.q, z0\.q, z1\.q
**	ret
*/
TEST_UNIFORM_Z (uzp2q_f16_tied1, svfloat16_t,
		z0 = svuzp2q_f16 (z0, z1),
		z0 = svuzp2q (z0, z1))

/*
** uzp2q_f16_tied2:
**	uzp2	z0\.q, z1\.q, z0\.q
**	ret
*/
TEST_UNIFORM_Z (uzp2q_f16_tied2, svfloat16_t,
		z0 = svuzp2q_f16 (z1, z0),
		z0 = svuzp2q (z1, z0))

/*
** uzp2q_f16_untied:
**	uzp2	z0\.q, z1\.q, z2\.q
**	ret
*/
TEST_UNIFORM_Z (uzp2q_f16_untied, svfloat16_t,
		z0 = svuzp2q_f16 (z1, z2),
		z0 = svuzp2q (z1, z2))
