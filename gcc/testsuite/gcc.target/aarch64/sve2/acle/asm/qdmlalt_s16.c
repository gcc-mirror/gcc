/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmlalt_s16_tied1:
**	sqdmlalt	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (qdmlalt_s16_tied1, svint16_t, svint8_t,
	     z0 = svqdmlalt_s16 (z0, z4, z5),
	     z0 = svqdmlalt (z0, z4, z5))

/*
** qdmlalt_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sqdmlalt	z0\.h, \1\.b, z1\.b
**	ret
*/
TEST_DUAL_Z_REV (qdmlalt_s16_tied2, svint16_t, svint8_t,
		 z0_res = svqdmlalt_s16 (z4, z0, z1),
		 z0_res = svqdmlalt (z4, z0, z1))

/*
** qdmlalt_s16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sqdmlalt	z0\.h, z1\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (qdmlalt_s16_tied3, svint16_t, svint8_t,
		 z0_res = svqdmlalt_s16 (z4, z1, z0),
		 z0_res = svqdmlalt (z4, z1, z0))

/*
** qdmlalt_s16_untied:
**	movprfx	z0, z1
**	sqdmlalt	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (qdmlalt_s16_untied, svint16_t, svint8_t,
	     z0 = svqdmlalt_s16 (z1, z4, z5),
	     z0 = svqdmlalt (z1, z4, z5))

/*
** qdmlalt_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	sqdmlalt	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_ZX (qdmlalt_w0_s16_tied1, svint16_t, svint8_t, int8_t,
	      z0 = svqdmlalt_n_s16 (z0, z4, x0),
	      z0 = svqdmlalt (z0, z4, x0))

/*
** qdmlalt_w0_s16_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	sqdmlalt	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_ZX (qdmlalt_w0_s16_untied, svint16_t, svint8_t, int8_t,
	      z0 = svqdmlalt_n_s16 (z1, z4, x0),
	      z0 = svqdmlalt (z1, z4, x0))

/*
** qdmlalt_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	sqdmlalt	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_Z (qdmlalt_11_s16_tied1, svint16_t, svint8_t,
	     z0 = svqdmlalt_n_s16 (z0, z4, 11),
	     z0 = svqdmlalt (z0, z4, 11))

/*
** qdmlalt_11_s16_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0, z1
**	sqdmlalt	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_Z (qdmlalt_11_s16_untied, svint16_t, svint8_t,
	     z0 = svqdmlalt_n_s16 (z1, z4, 11),
	     z0 = svqdmlalt (z1, z4, 11))
