/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"
#pragma GCC target "+sve2p3"
#pragma GCC target "+sme2p3"

/*
** abal_u16_tied1:
**	uabal	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (abal_u16_tied1, svuint16_t, svuint8_t,
	     z0 = svabal_u16 (z0, z4, z5),
	     z0 = svabal (z0, z4, z5))

/*
** abal_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uabal	z0\.h, \1\.b, z1\.b
**	ret
*/
TEST_DUAL_Z_REV (abal_u16_tied2, svuint16_t, svuint8_t,
		 z0_res = svabal_u16 (z4, z0, z1),
		 z0_res = svabal (z4, z0, z1))

/*
** abal_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uabal	z0\.h, z1\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (abal_u16_tied3, svuint16_t, svuint8_t,
		 z0_res = svabal_u16 (z4, z1, z0),
		 z0_res = svabal (z4, z1, z0))

/*
** abal_u16_untied:
**	movprfx	z0, z1
**	uabal	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (abal_u16_untied, svuint16_t, svuint8_t,
	     z0 = svabal_u16 (z1, z4, z5),
	     z0 = svabal (z1, z4, z5))

/*
** abal_w0_u16_tied1:
**	mov	(z[0-9]+\.b), w0
**	uabal	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_ZX (abal_w0_u16_tied1, svuint16_t, svuint8_t, uint8_t,
	      z0 = svabal_n_u16 (z0, z4, x0),
	      z0 = svabal (z0, z4, x0))

/*
** abal_w0_u16_untied:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	uabal	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_ZX (abal_w0_u16_untied, svuint16_t, svuint8_t, uint8_t,
	      z0 = svabal_n_u16 (z1, z4, x0),
	      z0 = svabal (z1, z4, x0))

/*
** abal_11_u16_tied1:
**	mov	(z[0-9]+\.b), #11
**	uabal	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_Z (abal_11_u16_tied1, svuint16_t, svuint8_t,
	     z0 = svabal_n_u16 (z0, z4, 11),
	     z0 = svabal (z0, z4, 11))

/*
** abal_11_u16_untied:
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0, z1
**	uabal	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_Z (abal_11_u16_untied, svuint16_t, svuint8_t,
	     z0 = svabal_n_u16 (z1, z4, 11),
	     z0 = svabal (z1, z4, 11))
