/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-require-effective-target aarch64_asm_f64mm_ok } */
/* { dg-additional-options "-march=armv8.2-a+f64mm" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mmla_f64_tied1:
**	fmmla	z0\.d, z4\.d, z5\.d
**	ret
*/
TEST_DUAL_Z (mmla_f64_tied1, svfloat64_t, svfloat64_t,
	     z0 = svmmla_f64 (z0, z4, z5),
	     z0 = svmmla (z0, z4, z5))

/*
** mmla_f64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	fmmla	z0\.d, \1, z1\.d
**	ret
*/
TEST_DUAL_Z_REV (mmla_f64_tied2, svfloat64_t, svfloat64_t,
		 z0_res = svmmla_f64 (z4, z0, z1),
		 z0_res = svmmla (z4, z0, z1))

/*
** mmla_f64_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	fmmla	z0\.d, z1\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (mmla_f64_tied3, svfloat64_t, svfloat64_t,
		 z0_res = svmmla_f64 (z4, z1, z0),
		 z0_res = svmmla (z4, z1, z0))

/*
** mmla_f64_untied:
**	movprfx	z0, z1
**	fmmla	z0\.d, z4\.d, z5\.d
**	ret
*/
TEST_DUAL_Z (mmla_f64_untied, svfloat64_t, svfloat64_t,
	     z0 = svmmla_f64 (z1, z4, z5),
	     z0 = svmmla (z1, z4, z5))
