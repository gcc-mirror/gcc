/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** orqv_d0_u8_tied:
**	orqv	v0\.16b, p0, z0\.b
**	ret
*/
TEST_REDUCTION_D (orqv_d0_u8_tied, uint8x16_t, svuint8_t,
		  d0 = svorqv_u8 (p0, z0),
		  d0 = svorqv (p0, z0))

/*
** orqv_d0_u8_untied:
**	orqv	v0\.16b, p0, z1\.b
**	ret
*/
TEST_REDUCTION_D (orqv_d0_u8_untied, uint8x16_t, svuint8_t,
		  d0 = svorqv_u8 (p0, z1),
		  d0 = svorqv (p0, z1))
