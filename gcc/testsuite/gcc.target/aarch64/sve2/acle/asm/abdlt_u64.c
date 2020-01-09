/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abdlt_u64_tied1:
**	uabdlt	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svabdlt_u64 (z0, z1),
		    z0_res = svabdlt (z0, z1))

/*
** abdlt_u64_tied2:
**	uabdlt	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_u64_tied2, svuint64_t, svuint32_t,
		    z0_res = svabdlt_u64 (z1, z0),
		    z0_res = svabdlt (z1, z0))

/*
** abdlt_u64_untied:
**	uabdlt	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svabdlt_u64 (z1, z2),
		    z0_res = svabdlt (z1, z2))

/*
** abdlt_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	uabdlt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlt_w0_u64_tied1, svuint64_t, svuint32_t, uint32_t,
		     z0_res = svabdlt_n_u64 (z0, x0),
		     z0_res = svabdlt (z0, x0))

/*
** abdlt_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	uabdlt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlt_w0_u64_untied, svuint64_t, svuint32_t, uint32_t,
		     z0_res = svabdlt_n_u64 (z1, x0),
		     z0_res = svabdlt (z1, x0))

/*
** abdlt_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	uabdlt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_11_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svabdlt_n_u64 (z0, 11),
		    z0_res = svabdlt (z0, 11))

/*
** abdlt_11_u64_untied:
**	mov	(z[0-9]+\.s), #11
**	uabdlt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_11_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svabdlt_n_u64 (z1, 11),
		    z0_res = svabdlt (z1, 11))
