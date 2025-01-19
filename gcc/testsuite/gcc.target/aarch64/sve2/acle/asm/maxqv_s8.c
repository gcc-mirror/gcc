/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** maxqv_d0_s8_tied:
**	smaxqv	v0\.16b, p0, z0\.b
**	ret
*/
TEST_REDUCTION_D (maxqv_d0_s8_tied, int8x16_t, svint8_t,
		  d0 = svmaxqv_s8 (p0, z0),
		  d0 = svmaxqv (p0, z0))

/*
** maxqv_d0_s8_untied:
**	smaxqv	v0\.16b, p0, z1\.b
**	ret
*/
TEST_REDUCTION_D (maxqv_d0_s8_untied, int8x16_t, svint8_t,
		  d0 = svmaxqv_s8 (p0, z1),
		  d0 = svmaxqv (p0, z1))
