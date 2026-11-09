/* { dg-do assemble { target aarch64_asm_sve-b16mm_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve-b16mm_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve-b16mm"

/*
** svmmla_bf16mm_tied:
**	bfmmla	z0\.h, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (svmmla_bf16mm_tied, svbfloat16_t, svbfloat16_t,
	     z0 = svmmla_bf16 (z0, z4, z5),
	     z0 = svmmla (z0, z4, z5))

/*
** svmmla_bf16mm:
**	movprfx	z0, z1
**	bfmmla	z0\.h, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (svmmla_bf16mm, svbfloat16_t, svbfloat16_t,
	     z0 = svmmla_bf16 (z1, z4, z5),
	     z0 = svmmla (z1, z4, z5))
