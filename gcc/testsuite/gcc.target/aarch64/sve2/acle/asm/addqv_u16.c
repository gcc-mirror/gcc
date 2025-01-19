/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** addqv_d0_u16_tied:
**	addqv	v0\.8h, p0, z0\.h
**	ret
*/
TEST_REDUCTION_D (addqv_d0_u16_tied, uint16x8_t, svuint16_t,
		  d0 = svaddqv_u16 (p0, z0),
		  d0 = svaddqv (p0, z0))

/*
** addqv_d0_u16_untied:
**	addqv	v0\.8h, p0, z1\.h
**	ret
*/
TEST_REDUCTION_D (addqv_d0_u16_untied, uint16x8_t, svuint16_t,
		  d0 = svaddqv_u16 (p0, z1),
		  d0 = svaddqv (p0, z1))
