/* { dg-do assemble { target aarch64_asm_sve-b16b16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve-b16b16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve-b16b16"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** clamp_bf16_tied1:
**	bfclamp	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_bf16_tied1, svbfloat16_t,
		z0 = svclamp_bf16 (z0, z1, z2),
		z0 = svclamp (z0, z1, z2))

/*
** clamp_bf16_tied2:
**	bfclamp	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_bf16_tied2, svbfloat16_t,
		z0 = svclamp_bf16 (z1, z0, z2),
		z0 = svclamp (z1, z0, z2))

/*
** clamp_bf16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	bfclamp	z0\.h, z2\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_bf16_tied3, svbfloat16_t,
		z0 = svclamp_bf16 (z1, z2, z0),
		z0 = svclamp (z1, z2, z0))

/*
** clamp_bf16_untied:
**	movprfx	z0, z1
**	bfclamp	z0\.h, z2\.h, z3\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_bf16_untied, svbfloat16_t,
		z0 = svclamp_bf16 (z1, z2, z3),
		z0 = svclamp (z1, z2, z3))
