/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** cvtb_untied:
**	ucvtf	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (cvtb_untied, svfloat16_t, svuint8_t,
		z0 = svcvtb_f16_u8 (z4),
		z0 = svcvtb_f16 (z4))

/*
** cvtb_tied:
**	ucvtf	z0\.h, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (cvtb_tied, svfloat16_t, svuint8_t,
		z0_res = svcvtb_f16_u8 (z0),
		z0_res = svcvtb_f16 (z0))
