/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** minnmqv_d0_f16_tied:
**	fminnmqv	v0\.8h, p0, z0\.h
**	ret
*/
TEST_REDUCTION_D (minnmqv_d0_f16_tied, float16x8_t, svfloat16_t,
		  d0 = svminnmqv_f16 (p0, z0),
		  d0 = svminnmqv (p0, z0))

/*
** minnmqv_d0_f16_untied:
**	fminnmqv	v0\.8h, p0, z1\.h
**	ret
*/
TEST_REDUCTION_D (minnmqv_d0_f16_untied, float16x8_t, svfloat16_t,
		  d0 = svminnmqv_f16 (p0, z1),
		  d0 = svminnmqv (p0, z1))
