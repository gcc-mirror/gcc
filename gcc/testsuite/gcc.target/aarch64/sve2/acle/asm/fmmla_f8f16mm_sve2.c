/* { dg-do assemble { target aarch64_asm_f8f16mm_ok } } */
/* { dg-do compile { target { ! aarch64_asm_f8f16mm_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

/* Binutils PR gas/33562 */
/* { dg-prune-output "SVE `movprfx' compatible instruction expected" } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2+f8f16mm"

/*
** svmmla_f16f8mm_tied:
**	msr	fpmr, x0
**	fmmla	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (svmmla_f16f8mm_tied, svfloat16_t, svmfloat8_t,
	     z0 = svmmla_f16_mf8_fpm (z0, z4, z5, fpm0),
	     z0 = svmmla_fpm (z0, z4, z5, fpm0))

/*
** svmmla_f16f8mm:
**	msr	fpmr, x0
**	movprfx	z0, z1
**	fmmla	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (svmmla_f16f8mm, svfloat16_t, svmfloat8_t,
	     z0 = svmmla_f16_mf8_fpm (z1, z4, z5, fpm0),
	     z0 = svmmla_fpm (z1, z4, z5, fpm0))

