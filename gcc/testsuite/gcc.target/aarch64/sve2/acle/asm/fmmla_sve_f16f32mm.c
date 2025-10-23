/* { dg-do assemble { target aarch64_asm_sve-f16f32mm_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve-f16f32mm_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

/* Binutils PR gas/33562 */
/* { dg-prune-output "SVE `movprfx' compatible instruction expected" } */

#include "test_sve_acle.h"

#pragma GCC target "+sve-f16f32mm"

/*
** svmmla_f32f16mm_tied:
**	fmmla	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (svmmla_f32f16mm_tied, svfloat32_t, svfloat16_t,
	     z0 = svmmla_f32_f16 (z0, z4, z5),
	     z0 = svmmla (z0, z4, z5))

/*
** svmmla_f32f16mm:
**	movprfx	z0, z1
**	fmmla	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (svmmla_f32f16mm, svfloat32_t, svfloat16_t,
	     z0 = svmmla_f32_f16 (z1, z4, z5),
	     z0 = svmmla (z1, z4, z5))

