/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mlalt_u32_tied1:
**	umlalt	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (mlalt_u32_tied1, svuint32_t, svuint16_t,
	     z0 = svmlalt_u32 (z0, z4, z5),
	     z0 = svmlalt (z0, z4, z5))

/*
** mlalt_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	umlalt	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (mlalt_u32_tied2, svuint32_t, svuint16_t,
		 z0_res = svmlalt_u32 (z4, z0, z1),
		 z0_res = svmlalt (z4, z0, z1))

/*
** mlalt_u32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	umlalt	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (mlalt_u32_tied3, svuint32_t, svuint16_t,
		 z0_res = svmlalt_u32 (z4, z1, z0),
		 z0_res = svmlalt (z4, z1, z0))

/*
** mlalt_u32_untied:
**	movprfx	z0, z1
**	umlalt	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (mlalt_u32_untied, svuint32_t, svuint16_t,
	     z0 = svmlalt_u32 (z1, z4, z5),
	     z0 = svmlalt (z1, z4, z5))

/*
** mlalt_w0_u32_tied1:
**	mov	(z[0-9]+\.h), w0
**	umlalt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (mlalt_w0_u32_tied1, svuint32_t, svuint16_t, uint16_t,
	      z0 = svmlalt_n_u32 (z0, z4, x0),
	      z0 = svmlalt (z0, z4, x0))

/*
** mlalt_w0_u32_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	umlalt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (mlalt_w0_u32_untied, svuint32_t, svuint16_t, uint16_t,
	      z0 = svmlalt_n_u32 (z1, z4, x0),
	      z0 = svmlalt (z1, z4, x0))

/*
** mlalt_11_u32_tied1:
**	mov	(z[0-9]+\.h), #11
**	umlalt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (mlalt_11_u32_tied1, svuint32_t, svuint16_t,
	     z0 = svmlalt_n_u32 (z0, z4, 11),
	     z0 = svmlalt (z0, z4, 11))

/*
** mlalt_11_u32_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0, z1
**	umlalt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (mlalt_11_u32_untied, svuint32_t, svuint16_t,
	     z0 = svmlalt_n_u32 (z1, z4, 11),
	     z0 = svmlalt (z1, z4, 11))
