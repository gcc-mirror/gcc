/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mlalt_u16_tied1:
**	umlalt	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (mlalt_u16_tied1, svuint16_t, svuint8_t,
	     z0 = svmlalt_u16 (z0, z4, z5),
	     z0 = svmlalt (z0, z4, z5))

/*
** mlalt_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	umlalt	z0\.h, \1\.b, z1\.b
**	ret
*/
TEST_DUAL_Z_REV (mlalt_u16_tied2, svuint16_t, svuint8_t,
		 z0_res = svmlalt_u16 (z4, z0, z1),
		 z0_res = svmlalt (z4, z0, z1))

/*
** mlalt_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	umlalt	z0\.h, z1\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (mlalt_u16_tied3, svuint16_t, svuint8_t,
		 z0_res = svmlalt_u16 (z4, z1, z0),
		 z0_res = svmlalt (z4, z1, z0))

/*
** mlalt_u16_untied:
**	movprfx	z0, z1
**	umlalt	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (mlalt_u16_untied, svuint16_t, svuint8_t,
	     z0 = svmlalt_u16 (z1, z4, z5),
	     z0 = svmlalt (z1, z4, z5))

/*
** mlalt_w0_u16_tied1:
**	mov	(z[0-9]+\.b), w0
**	umlalt	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_ZX (mlalt_w0_u16_tied1, svuint16_t, svuint8_t, uint8_t,
	      z0 = svmlalt_n_u16 (z0, z4, x0),
	      z0 = svmlalt (z0, z4, x0))

/*
** mlalt_w0_u16_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	umlalt	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_ZX (mlalt_w0_u16_untied, svuint16_t, svuint8_t, uint8_t,
	      z0 = svmlalt_n_u16 (z1, z4, x0),
	      z0 = svmlalt (z1, z4, x0))

/*
** mlalt_11_u16_tied1:
**	mov	(z[0-9]+\.b), #11
**	umlalt	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_Z (mlalt_11_u16_tied1, svuint16_t, svuint8_t,
	     z0 = svmlalt_n_u16 (z0, z4, 11),
	     z0 = svmlalt (z0, z4, 11))

/*
** mlalt_11_u16_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0, z1
**	umlalt	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_Z (mlalt_11_u16_untied, svuint16_t, svuint8_t,
	     z0 = svmlalt_n_u16 (z1, z4, 11),
	     z0 = svmlalt (z1, z4, 11))
