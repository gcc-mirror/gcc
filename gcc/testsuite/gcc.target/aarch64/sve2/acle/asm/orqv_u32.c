/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** orqv_d0_u32_tied:
**	orqv	v0\.4s, p0, z0\.s
**	ret
*/
TEST_REDUCTION_D (orqv_d0_u32_tied, uint32x4_t, svuint32_t,
		  d0 = svorqv_u32 (p0, z0),
		  d0 = svorqv (p0, z0))

/*
** orqv_d0_u32_untied:
**	orqv	v0\.4s, p0, z1\.s
**	ret
*/
TEST_REDUCTION_D (orqv_d0_u32_untied, uint32x4_t, svuint32_t,
		  d0 = svorqv_u32 (p0, z1),
		  d0 = svorqv (p0, z1))
