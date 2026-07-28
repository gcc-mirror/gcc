/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** cvtt_untied:
**	scvtflt	z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (cvtt_untied, svfloat64_t, svint32_t,
		z0 = svcvtt_f64_s32 (z4),
		z0 = svcvtt_f64 (z4))

/*
** cvtt_tied:
**	scvtflt	z0\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (cvtt_tied, svfloat64_t, svint32_t,
		z0_res = svcvtt_f64_s32 (z0),
		z0_res = svcvtt_f64 (z0))
