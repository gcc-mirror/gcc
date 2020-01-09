/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abalt_s32_tied1:
**	sabalt	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (abalt_s32_tied1, svint32_t, svint16_t,
	     z0 = svabalt_s32 (z0, z4, z5),
	     z0 = svabalt (z0, z4, z5))

/*
** abalt_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sabalt	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (abalt_s32_tied2, svint32_t, svint16_t,
		 z0_res = svabalt_s32 (z4, z0, z1),
		 z0_res = svabalt (z4, z0, z1))

/*
** abalt_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sabalt	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (abalt_s32_tied3, svint32_t, svint16_t,
		 z0_res = svabalt_s32 (z4, z1, z0),
		 z0_res = svabalt (z4, z1, z0))

/*
** abalt_s32_untied:
**	movprfx	z0, z1
**	sabalt	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (abalt_s32_untied, svint32_t, svint16_t,
	     z0 = svabalt_s32 (z1, z4, z5),
	     z0 = svabalt (z1, z4, z5))

/*
** abalt_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	sabalt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (abalt_w0_s32_tied1, svint32_t, svint16_t, int16_t,
	      z0 = svabalt_n_s32 (z0, z4, x0),
	      z0 = svabalt (z0, z4, x0))

/*
** abalt_w0_s32_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	sabalt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (abalt_w0_s32_untied, svint32_t, svint16_t, int16_t,
	      z0 = svabalt_n_s32 (z1, z4, x0),
	      z0 = svabalt (z1, z4, x0))

/*
** abalt_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	sabalt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (abalt_11_s32_tied1, svint32_t, svint16_t,
	     z0 = svabalt_n_s32 (z0, z4, 11),
	     z0 = svabalt (z0, z4, 11))

/*
** abalt_11_s32_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0, z1
**	sabalt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (abalt_11_s32_untied, svint32_t, svint16_t,
	     z0 = svabalt_n_s32 (z1, z4, 11),
	     z0 = svabalt (z1, z4, 11))
