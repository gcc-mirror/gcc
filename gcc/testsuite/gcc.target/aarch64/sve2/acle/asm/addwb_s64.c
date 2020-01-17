/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addwb_s64_tied1:
**	saddwb	z0\.d, z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (addwb_s64_tied1, svint64_t, svint32_t,
	     z0 = svaddwb_s64 (z0, z4),
	     z0 = svaddwb (z0, z4))

/*
** addwb_s64_tied2:
**	saddwb	z0\.d, z4\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (addwb_s64_tied2, svint64_t, svint32_t,
		 z0_res = svaddwb_s64 (z4, z0),
		 z0_res = svaddwb (z4, z0))

/*
** addwb_s64_untied:
**	saddwb	z0\.d, z1\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (addwb_s64_untied, svint64_t, svint32_t,
	     z0 = svaddwb_s64 (z1, z4),
	     z0 = svaddwb (z1, z4))

/*
** addwb_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	saddwb	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (addwb_w0_s64_tied1, svint64_t, int32_t,
		 z0 = svaddwb_n_s64 (z0, x0),
		 z0 = svaddwb (z0, x0))

/*
** addwb_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	saddwb	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (addwb_w0_s64_untied, svint64_t, int32_t,
		 z0 = svaddwb_n_s64 (z1, x0),
		 z0 = svaddwb (z1, x0))

/*
** addwb_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	saddwb	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (addwb_11_s64_tied1, svint64_t,
		z0 = svaddwb_n_s64 (z0, 11),
		z0 = svaddwb (z0, 11))

/*
** addwb_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	saddwb	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (addwb_11_s64_untied, svint64_t,
		z0 = svaddwb_n_s64 (z1, 11),
		z0 = svaddwb (z1, 11))
