/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abdlt_s64_tied1:
**	sabdlt	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_s64_tied1, svint64_t, svint32_t,
		    z0_res = svabdlt_s64 (z0, z1),
		    z0_res = svabdlt (z0, z1))

/*
** abdlt_s64_tied2:
**	sabdlt	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_s64_tied2, svint64_t, svint32_t,
		    z0_res = svabdlt_s64 (z1, z0),
		    z0_res = svabdlt (z1, z0))

/*
** abdlt_s64_untied:
**	sabdlt	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_s64_untied, svint64_t, svint32_t,
		    z0_res = svabdlt_s64 (z1, z2),
		    z0_res = svabdlt (z1, z2))

/*
** abdlt_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	sabdlt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlt_w0_s64_tied1, svint64_t, svint32_t, int32_t,
		     z0_res = svabdlt_n_s64 (z0, x0),
		     z0_res = svabdlt (z0, x0))

/*
** abdlt_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	sabdlt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlt_w0_s64_untied, svint64_t, svint32_t, int32_t,
		     z0_res = svabdlt_n_s64 (z1, x0),
		     z0_res = svabdlt (z1, x0))

/*
** abdlt_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	sabdlt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_11_s64_tied1, svint64_t, svint32_t,
		    z0_res = svabdlt_n_s64 (z0, 11),
		    z0_res = svabdlt (z0, 11))

/*
** abdlt_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	sabdlt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_11_s64_untied, svint64_t, svint32_t,
		    z0_res = svabdlt_n_s64 (z1, 11),
		    z0_res = svabdlt (z1, 11))
