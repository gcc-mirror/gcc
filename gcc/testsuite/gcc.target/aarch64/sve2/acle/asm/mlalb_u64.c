/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mlalb_u64_tied1:
**	umlalb	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (mlalb_u64_tied1, svuint64_t, svuint32_t,
	     z0 = svmlalb_u64 (z0, z4, z5),
	     z0 = svmlalb (z0, z4, z5))

/*
** mlalb_u64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	umlalb	z0\.d, \1\.s, z1\.s
**	ret
*/
TEST_DUAL_Z_REV (mlalb_u64_tied2, svuint64_t, svuint32_t,
		 z0_res = svmlalb_u64 (z4, z0, z1),
		 z0_res = svmlalb (z4, z0, z1))

/*
** mlalb_u64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	umlalb	z0\.d, z1\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (mlalb_u64_tied3, svuint64_t, svuint32_t,
		 z0_res = svmlalb_u64 (z4, z1, z0),
		 z0_res = svmlalb (z4, z1, z0))

/*
** mlalb_u64_untied:
**	movprfx	z0, z1
**	umlalb	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (mlalb_u64_untied, svuint64_t, svuint32_t,
	     z0 = svmlalb_u64 (z1, z4, z5),
	     z0 = svmlalb (z1, z4, z5))

/*
** mlalb_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	umlalb	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (mlalb_w0_u64_tied1, svuint64_t, svuint32_t, uint32_t,
	      z0 = svmlalb_n_u64 (z0, z4, x0),
	      z0 = svmlalb (z0, z4, x0))

/*
** mlalb_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	umlalb	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (mlalb_w0_u64_untied, svuint64_t, svuint32_t, uint32_t,
	      z0 = svmlalb_n_u64 (z1, z4, x0),
	      z0 = svmlalb (z1, z4, x0))

/*
** mlalb_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	umlalb	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (mlalb_11_u64_tied1, svuint64_t, svuint32_t,
	     z0 = svmlalb_n_u64 (z0, z4, 11),
	     z0 = svmlalb (z0, z4, 11))

/*
** mlalb_11_u64_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	umlalb	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (mlalb_11_u64_untied, svuint64_t, svuint32_t,
	     z0 = svmlalb_n_u64 (z1, z4, 11),
	     z0 = svmlalb (z1, z4, 11))
