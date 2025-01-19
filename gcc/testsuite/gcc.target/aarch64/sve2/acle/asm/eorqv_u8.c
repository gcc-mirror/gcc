/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** eorqv_d0_u8_tied:
**	eorqv	v0\.16b, p0, z0\.b
**	ret
*/
TEST_REDUCTION_D (eorqv_d0_u8_tied, uint8x16_t, svuint8_t,
		  d0 = sveorqv_u8 (p0, z0),
		  d0 = sveorqv (p0, z0))

/*
** eorqv_d0_u8_untied:
**	eorqv	v0\.16b, p0, z1\.b
**	ret
*/
TEST_REDUCTION_D (eorqv_d0_u8_untied, uint8x16_t, svuint8_t,
		  d0 = sveorqv_u8 (p0, z1),
		  d0 = sveorqv (p0, z1))
