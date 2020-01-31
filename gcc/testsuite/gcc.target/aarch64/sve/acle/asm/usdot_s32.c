/* { dg-require-effective-target aarch64_asm_i8mm_ok } */
/* { dg-additional-options "-march=armv8.2-a+sve+i8mm" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** usdot_s32_tied1:
**	usdot	z0\.s, z2\.b, z4\.b
**	ret
*/
TEST_TRIPLE_Z (usdot_s32_tied1, svint32_t, svuint8_t, svint8_t,
	       z0 = svusdot_s32 (z0, z2, z4),
	       z0 = svusdot (z0, z2, z4))

/*
** usdot_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	usdot	z0\.s, z2\.b, \1\.b
**	ret
*/
TEST_TRIPLE_Z_REV (usdot_s32_tied2, svint32_t, svuint8_t, svint8_t,
		   z0_res = svusdot_s32 (z4, z2, z0),
		   z0_res = svusdot (z4, z2, z0))

/*
** usdot_w0_s32_tied:
**	mov	(z[0-9]+\.b), w0
**	usdot	z0\.s, z2\.b, \1
**	ret
*/
TEST_TRIPLE_ZX (usdot_w0_s32_tied, svint32_t, svuint8_t, int8_t,
		z0 = svusdot_n_s32 (z0, z2, x0),
		z0 = svusdot (z0, z2, x0))

/*
** usdot_9_s32_tied:
**	mov	(z[0-9]+\.b), #9
**	usdot	z0\.s, z2\.b, \1
**	ret
*/
TEST_TRIPLE_Z (usdot_9_s32_tied, svint32_t, svuint8_t, int8_t,
	       z0 = svusdot_n_s32 (z0, z2, 9),
	       z0 = svusdot (z0, z2, 9))

