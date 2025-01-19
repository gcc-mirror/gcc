/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** minqv_d0_f32_tied:
**	fminqv	v0\.4s, p0, z0\.s
**	ret
*/
TEST_REDUCTION_D (minqv_d0_f32_tied, float32x4_t, svfloat32_t,
		  d0 = svminqv_f32 (p0, z0),
		  d0 = svminqv (p0, z0))

/*
** minqv_d0_f32_untied:
**	fminqv	v0\.4s, p0, z1\.s
**	ret
*/
TEST_REDUCTION_D (minqv_d0_f32_untied, float32x4_t, svfloat32_t,
		  d0 = svminqv_f32 (p0, z1),
		  d0 = svminqv (p0, z1))
