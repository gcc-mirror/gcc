/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-require-effective-target aarch64_asm_f64mm_ok } */
/* { dg-additional-options "-march=armv8.2-a+f64mm" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uzp1q_bf16_tied1:
**	uzp1	z0\.q, z0\.q, z1\.q
**	ret
*/
TEST_UNIFORM_Z (uzp1q_bf16_tied1, svbfloat16_t,
		z0 = svuzp1q_bf16 (z0, z1),
		z0 = svuzp1q (z0, z1))

/*
** uzp1q_bf16_tied2:
**	uzp1	z0\.q, z1\.q, z0\.q
**	ret
*/
TEST_UNIFORM_Z (uzp1q_bf16_tied2, svbfloat16_t,
		z0 = svuzp1q_bf16 (z1, z0),
		z0 = svuzp1q (z1, z0))

/*
** uzp1q_bf16_untied:
**	uzp1	z0\.q, z1\.q, z2\.q
**	ret
*/
TEST_UNIFORM_Z (uzp1q_bf16_untied, svbfloat16_t,
		z0 = svuzp1q_bf16 (z1, z2),
		z0 = svuzp1q (z1, z2))
