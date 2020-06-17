/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmlalbt_s32_tied1:
**	sqdmlalbt	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (qdmlalbt_s32_tied1, svint32_t, svint16_t,
	     z0 = svqdmlalbt_s32 (z0, z4, z5),
	     z0 = svqdmlalbt (z0, z4, z5))

/*
** qdmlalbt_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sqdmlalbt	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (qdmlalbt_s32_tied2, svint32_t, svint16_t,
		 z0_res = svqdmlalbt_s32 (z4, z0, z1),
		 z0_res = svqdmlalbt (z4, z0, z1))

/*
** qdmlalbt_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sqdmlalbt	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (qdmlalbt_s32_tied3, svint32_t, svint16_t,
		 z0_res = svqdmlalbt_s32 (z4, z1, z0),
		 z0_res = svqdmlalbt (z4, z1, z0))

/*
** qdmlalbt_s32_untied:
**	movprfx	z0, z1
**	sqdmlalbt	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (qdmlalbt_s32_untied, svint32_t, svint16_t,
	     z0 = svqdmlalbt_s32 (z1, z4, z5),
	     z0 = svqdmlalbt (z1, z4, z5))

/*
** qdmlalbt_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqdmlalbt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (qdmlalbt_w0_s32_tied1, svint32_t, svint16_t, int16_t,
	      z0 = svqdmlalbt_n_s32 (z0, z4, x0),
	      z0 = svqdmlalbt (z0, z4, x0))

/*
** qdmlalbt_w0_s32_untied: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	sqdmlalbt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (qdmlalbt_w0_s32_untied, svint32_t, svint16_t, int16_t,
	      z0 = svqdmlalbt_n_s32 (z1, z4, x0),
	      z0 = svqdmlalbt (z1, z4, x0))

/*
** qdmlalbt_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	sqdmlalbt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (qdmlalbt_11_s32_tied1, svint32_t, svint16_t,
	     z0 = svqdmlalbt_n_s32 (z0, z4, 11),
	     z0 = svqdmlalbt (z0, z4, 11))

/*
** qdmlalbt_11_s32_untied: { xfail *-*-*}
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0, z1
**	sqdmlalbt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (qdmlalbt_11_s32_untied, svint32_t, svint16_t,
	     z0 = svqdmlalbt_n_s32 (z1, z4, 11),
	     z0 = svqdmlalbt (z1, z4, 11))
