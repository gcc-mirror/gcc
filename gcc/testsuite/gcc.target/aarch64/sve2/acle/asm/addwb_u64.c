/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addwb_u64_tied1:
**	uaddwb	z0\.d, z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (addwb_u64_tied1, svuint64_t, svuint32_t,
	     z0 = svaddwb_u64 (z0, z4),
	     z0 = svaddwb (z0, z4))

/*
** addwb_u64_tied2:
**	uaddwb	z0\.d, z4\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (addwb_u64_tied2, svuint64_t, svuint32_t,
		 z0_res = svaddwb_u64 (z4, z0),
		 z0_res = svaddwb (z4, z0))

/*
** addwb_u64_untied:
**	uaddwb	z0\.d, z1\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (addwb_u64_untied, svuint64_t, svuint32_t,
	     z0 = svaddwb_u64 (z1, z4),
	     z0 = svaddwb (z1, z4))

/*
** addwb_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	uaddwb	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (addwb_w0_u64_tied1, svuint64_t, uint32_t,
		 z0 = svaddwb_n_u64 (z0, x0),
		 z0 = svaddwb (z0, x0))

/*
** addwb_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	uaddwb	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (addwb_w0_u64_untied, svuint64_t, uint32_t,
		 z0 = svaddwb_n_u64 (z1, x0),
		 z0 = svaddwb (z1, x0))

/*
** addwb_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	uaddwb	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (addwb_11_u64_tied1, svuint64_t,
		z0 = svaddwb_n_u64 (z0, 11),
		z0 = svaddwb (z0, 11))

/*
** addwb_11_u64_untied:
**	mov	(z[0-9]+\.s), #11
**	uaddwb	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (addwb_11_u64_untied, svuint64_t,
		z0 = svaddwb_n_u64 (z1, 11),
		z0 = svaddwb (z1, 11))
