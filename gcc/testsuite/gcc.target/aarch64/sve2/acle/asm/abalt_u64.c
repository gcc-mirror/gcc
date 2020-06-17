/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abalt_u64_tied1:
**	uabalt	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (abalt_u64_tied1, svuint64_t, svuint32_t,
	     z0 = svabalt_u64 (z0, z4, z5),
	     z0 = svabalt (z0, z4, z5))

/*
** abalt_u64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uabalt	z0\.d, \1\.s, z1\.s
**	ret
*/
TEST_DUAL_Z_REV (abalt_u64_tied2, svuint64_t, svuint32_t,
		 z0_res = svabalt_u64 (z4, z0, z1),
		 z0_res = svabalt (z4, z0, z1))

/*
** abalt_u64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uabalt	z0\.d, z1\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (abalt_u64_tied3, svuint64_t, svuint32_t,
		 z0_res = svabalt_u64 (z4, z1, z0),
		 z0_res = svabalt (z4, z1, z0))

/*
** abalt_u64_untied:
**	movprfx	z0, z1
**	uabalt	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (abalt_u64_untied, svuint64_t, svuint32_t,
	     z0 = svabalt_u64 (z1, z4, z5),
	     z0 = svabalt (z1, z4, z5))

/*
** abalt_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	uabalt	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (abalt_w0_u64_tied1, svuint64_t, svuint32_t, uint32_t,
	      z0 = svabalt_n_u64 (z0, z4, x0),
	      z0 = svabalt (z0, z4, x0))

/*
** abalt_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	uabalt	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (abalt_w0_u64_untied, svuint64_t, svuint32_t, uint32_t,
	      z0 = svabalt_n_u64 (z1, z4, x0),
	      z0 = svabalt (z1, z4, x0))

/*
** abalt_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	uabalt	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (abalt_11_u64_tied1, svuint64_t, svuint32_t,
	     z0 = svabalt_n_u64 (z0, z4, 11),
	     z0 = svabalt (z0, z4, 11))

/*
** abalt_11_u64_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	uabalt	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (abalt_11_u64_untied, svuint64_t, svuint32_t,
	     z0 = svabalt_n_u64 (z1, z4, 11),
	     z0 = svabalt (z1, z4, 11))
