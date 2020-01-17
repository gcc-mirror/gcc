/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addhnb_s64_tied1:
**	addhnb	z0\.s, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_s64_tied1, svint32_t, svint64_t,
		    z0_res = svaddhnb_s64 (z0, z1),
		    z0_res = svaddhnb (z0, z1))

/*
** addhnb_s64_tied2:
**	addhnb	z0\.s, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_s64_tied2, svint32_t, svint64_t,
		    z0_res = svaddhnb_s64 (z1, z0),
		    z0_res = svaddhnb (z1, z0))

/*
** addhnb_s64_untied:
**	addhnb	z0\.s, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_s64_untied, svint32_t, svint64_t,
		    z0_res = svaddhnb_s64 (z1, z2),
		    z0_res = svaddhnb (z1, z2))

/*
** addhnb_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	addhnb	z0\.s, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_TYPE_CHANGE_ZX (addhnb_x0_s64_tied1, svint32_t, svint64_t, int64_t,
		     z0_res = svaddhnb_n_s64 (z0, x0),
		     z0_res = svaddhnb (z0, x0))

/*
** addhnb_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
**	addhnb	z0\.s, (z1\.d, \1|\1, z1\.d)
**	ret
*/
TEST_TYPE_CHANGE_ZX (addhnb_x0_s64_untied, svint32_t, svint64_t, int64_t,
		     z0_res = svaddhnb_n_s64 (z1, x0),
		     z0_res = svaddhnb (z1, x0))

/*
** addhnb_11_s64_tied1:
**	mov	(z[0-9]+\.d), #11
**	addhnb	z0\.s, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_11_s64_tied1, svint32_t, svint64_t,
		    z0_res = svaddhnb_n_s64 (z0, 11),
		    z0_res = svaddhnb (z0, 11))

/*
** addhnb_11_s64_untied:
**	mov	(z[0-9]+\.d), #11
**	addhnb	z0\.s, (z1\.d, \1|\1, z1\.d)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_11_s64_untied, svint32_t, svint64_t,
		    z0_res = svaddhnb_n_s64 (z1, 11),
		    z0_res = svaddhnb (z1, 11))
