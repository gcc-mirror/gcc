/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** andqv_d0_s32_tied:
**	andqv	v0\.4s, p0, z0\.s
**	ret
*/
TEST_REDUCTION_D (andqv_d0_s32_tied, int32x4_t, svint32_t,
		  d0 = svandqv_s32 (p0, z0),
		  d0 = svandqv (p0, z0))

/*
** andqv_d0_s32_untied:
**	andqv	v0\.4s, p0, z1\.s
**	ret
*/
TEST_REDUCTION_D (andqv_d0_s32_untied, int32x4_t, svint32_t,
		  d0 = svandqv_s32 (p0, z1),
		  d0 = svandqv (p0, z1))
