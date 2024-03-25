/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-require-effective-target aarch64_asm_i8mm_ok } */
/* { dg-additional-options "-march=armv8.2-a+sve+i8mm" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** usmmla_s32_tied1:
**	usmmla	z0\.s, z2\.b, z4\.b
**	ret
*/
TEST_TRIPLE_Z (usmmla_s32_tied1, svint32_t, svuint8_t, svint8_t,
	       z0 = svusmmla_s32 (z0, z2, z4),
	       z0 = svusmmla (z0, z2, z4))

/*
** usmmla_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z2
**	usmmla	z0\.s, \1\.b, z4\.b
**	ret
*/
TEST_TRIPLE_Z_REV2 (usmmla_s32_tied2, svint32_t, svuint8_t, svint8_t,
		    z0_res = svusmmla_s32 (z2, z0, z4),
		    z0_res = svusmmla (z2, z0, z4))

/*
** usmmla_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	usmmla	z0\.s, z2\.b, \1\.b
**	ret
*/
TEST_TRIPLE_Z_REV (usmmla_s32_tied3, svint32_t, svuint8_t, svint8_t,
		   z0_res = svusmmla_s32 (z4, z2, z0),
		   z0_res = svusmmla (z4, z2, z0))

/*
** usmmla_s32_untied:
**	movprfx	z0, z1
**	usmmla	z0\.s, z2\.b, z4\.b
**	ret
*/
TEST_TRIPLE_Z (usmmla_s32_untied, svint32_t, svuint8_t, svint8_t,
	       z0 = svusmmla_s32 (z1, z2, z4),
	       z0 = svusmmla (z1, z2, z4))
