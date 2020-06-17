/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mlalb_s32_tied1:
**	smlalb	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (mlalb_s32_tied1, svint32_t, svint16_t,
	     z0 = svmlalb_s32 (z0, z4, z5),
	     z0 = svmlalb (z0, z4, z5))

/*
** mlalb_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	smlalb	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (mlalb_s32_tied2, svint32_t, svint16_t,
		 z0_res = svmlalb_s32 (z4, z0, z1),
		 z0_res = svmlalb (z4, z0, z1))

/*
** mlalb_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	smlalb	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (mlalb_s32_tied3, svint32_t, svint16_t,
		 z0_res = svmlalb_s32 (z4, z1, z0),
		 z0_res = svmlalb (z4, z1, z0))

/*
** mlalb_s32_untied:
**	movprfx	z0, z1
**	smlalb	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (mlalb_s32_untied, svint32_t, svint16_t,
	     z0 = svmlalb_s32 (z1, z4, z5),
	     z0 = svmlalb (z1, z4, z5))

/*
** mlalb_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	smlalb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (mlalb_w0_s32_tied1, svint32_t, svint16_t, int16_t,
	      z0 = svmlalb_n_s32 (z0, z4, x0),
	      z0 = svmlalb (z0, z4, x0))

/*
** mlalb_w0_s32_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	smlalb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (mlalb_w0_s32_untied, svint32_t, svint16_t, int16_t,
	      z0 = svmlalb_n_s32 (z1, z4, x0),
	      z0 = svmlalb (z1, z4, x0))

/*
** mlalb_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	smlalb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (mlalb_11_s32_tied1, svint32_t, svint16_t,
	     z0 = svmlalb_n_s32 (z0, z4, 11),
	     z0 = svmlalb (z0, z4, 11))

/*
** mlalb_11_s32_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0, z1
**	smlalb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (mlalb_11_s32_untied, svint32_t, svint16_t,
	     z0 = svmlalb_n_s32 (z1, z4, 11),
	     z0 = svmlalb (z1, z4, 11))
