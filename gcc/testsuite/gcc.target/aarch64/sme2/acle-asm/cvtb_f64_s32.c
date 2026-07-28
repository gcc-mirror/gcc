/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** cvtb_untied:
**	scvtf	z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (cvtb_untied, svfloat64_t, svint32_t,
		z0 = svcvtb_f64_s32 (z4),
		z0 = svcvtb_f64 (z4))

/*
** cvtb_tied:
**	scvtf	z0\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (cvtb_tied, svfloat64_t, svint32_t,
		z0_res = svcvtb_f64_s32 (z0),
		z0_res = svcvtb_f64 (z0))
