/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** cvtb_untied:
**	scvtf	z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (cvtb_untied, svfloat32_t, svint16_t,
		z0 = svcvtb_f32_s16 (z4),
		z0 = svcvtb_f32 (z4))

/*
** cvtb_tied:
**	scvtf	z0\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (cvtb_tied, svfloat32_t, svint16_t,
		z0_res = svcvtb_f32_s16 (z0),
		z0_res = svcvtb_f32 (z0))
