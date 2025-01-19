/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** maxnmqv_d0_f16_tied:
**	fmaxnmqv	v0\.8h, p0, z0\.h
**	ret
*/
TEST_REDUCTION_D (maxnmqv_d0_f16_tied, float16x8_t, svfloat16_t,
		  d0 = svmaxnmqv_f16 (p0, z0),
		  d0 = svmaxnmqv (p0, z0))

/*
** maxnmqv_d0_f16_untied:
**	fmaxnmqv	v0\.8h, p0, z1\.h
**	ret
*/
TEST_REDUCTION_D (maxnmqv_d0_f16_untied, float16x8_t, svfloat16_t,
		  d0 = svmaxnmqv_f16 (p0, z1),
		  d0 = svmaxnmqv (p0, z1))
