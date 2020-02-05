/* { dg-require-effective-target aarch64_asm_i8mm_ok } */
/* { dg-additional-options "-march=armv8.2-a+sve+i8mm" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mmla_u32_tied1:
**	ummla	z0\.s, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (mmla_u32_tied1, svuint32_t, svuint8_t,
	     z0 = svmmla_u32 (z0, z4, z5),
	     z0 = svmmla (z0, z4, z5))

/*
** mmla_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	ummla	z0\.s, \1\.b, z1\.b
**	ret
*/
TEST_DUAL_Z_REV (mmla_u32_tied2, svuint32_t, svuint8_t,
		 z0_res = svmmla_u32 (z4, z0, z1),
		 z0_res = svmmla (z4, z0, z1))

/*
** mmla_u32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	ummla	z0\.s, z1\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (mmla_u32_tied3, svuint32_t, svuint8_t,
		 z0_res = svmmla_u32 (z4, z1, z0),
		 z0_res = svmmla (z4, z1, z0))

/*
** mmla_u32_untied:
**	movprfx	z0, z1
**	ummla	z0\.s, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (mmla_u32_untied, svuint32_t, svuint8_t,
	     z0 = svmmla_u32 (z1, z4, z5),
	     z0 = svmmla (z1, z4, z5))
