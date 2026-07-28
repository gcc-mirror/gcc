/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** dot_u16_tied1:
**	udot	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (dot_u16_tied1, svuint16_t, svuint8_t,
	     z0 = svdot_u16_u8 (z0, z4, z5),
	     z0 = svdot (z0, z4, z5))

/*
** dot_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	udot	z0\.h, \1\.b, z1\.b
**	ret
*/
TEST_DUAL_Z_REV (dot_u16_tied2, svuint16_t, svuint8_t,
		 z0_res = svdot_u16_u8 (z4, z0, z1),
		 z0_res = svdot (z4, z0, z1))

/*
** dot_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	udot	z0\.h, z1\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (dot_u16_tied3, svuint16_t, svuint8_t,
		 z0_res = svdot_u16_u8 (z4, z1, z0),
		 z0_res = svdot (z4, z1, z0))

/*
** dot_u16_untied:
**	movprfx	z0, z1
**	udot	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (dot_u16_untied, svuint16_t, svuint8_t,
	     z0 = svdot_u16_u8 (z1, z4, z5),
	     z0 = svdot (z1, z4, z5))