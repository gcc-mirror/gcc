/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** orqv_d0_s16_tied:
**	orqv	v0\.8h, p0, z0\.h
**	ret
*/
TEST_REDUCTION_D (orqv_d0_s16_tied, int16x8_t, svint16_t,
		  d0 = svorqv_s16 (p0, z0),
		  d0 = svorqv (p0, z0))

/*
** orqv_d0_s16_untied:
**	orqv	v0\.8h, p0, z1\.h
**	ret
*/
TEST_REDUCTION_D (orqv_d0_s16_untied, int16x8_t, svint16_t,
		  d0 = svorqv_s16 (p0, z1),
		  d0 = svorqv (p0, z1))
