/* { dg-require-effective-target aarch64_asm_i8mm_ok } */
/* { dg-additional-options "-march=armv8.2-a+sve+i8mm" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sudot_s32_tied1:
**	usdot	z0\.s, z4\.b, z2\.b
**	ret
*/
TEST_TRIPLE_Z (sudot_s32_tied1, svint32_t, svint8_t, svuint8_t,
	       z0 = svsudot_s32 (z0, z2, z4),
	       z0 = svsudot (z0, z2, z4))

/*
** sudot_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	usdot	z0\.s, \1\.b, z2\.b
**	ret
*/
TEST_TRIPLE_Z_REV (sudot_s32_tied2, svint32_t, svint8_t, svuint8_t,
		   z0_res = svsudot_s32 (z4, z2, z0),
		   z0_res = svsudot (z4, z2, z0))

/*
** sudot_w0_s32_tied:
**	mov	(z[0-9]+\.b), w0
**	usdot	z0\.s, \1, z2\.b
**	ret
*/
TEST_TRIPLE_ZX (sudot_w0_s32_tied, svint32_t, svint8_t, uint8_t,
	       z0 = svsudot_n_s32 (z0, z2, x0),
	       z0 = svsudot (z0, z2, x0))

/*
** sudot_9_s32_tied:
**	mov	(z[0-9]+\.b), #9
**	usdot	z0\.s, \1, z2\.b
**	ret
*/
TEST_TRIPLE_Z (sudot_9_s32_tied, svint32_t, svint8_t, uint8_t,
	       z0 = svsudot_n_s32 (z0, z2, 9),
	       z0 = svsudot (z0, z2, 9))
