/* { dg-do assemble { target { aarch64_asm_sve2p2_ok && aarch64_asm_f16mm_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sve2p2_ok && aarch64_asm_f16mm_ok } } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p2+f16mm"

/*
** svmmla_f16mm_tied:
**	fmmla	z0\.h, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (svmmla_f16mm_tied, svfloat16_t, svfloat16_t,
	     z0 = svmmla_f16 (z0, z4, z5),
	     z0 = svmmla (z0, z4, z5))

/*
** svmmla_f16mm:
**	movprfx	z0, z1
**	fmmla	z0\.h, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (svmmla_f16mm, svfloat16_t, svfloat16_t,
	     z0 = svmmla_f16 (z1, z4, z5),
	     z0 = svmmla (z1, z4, z5))
