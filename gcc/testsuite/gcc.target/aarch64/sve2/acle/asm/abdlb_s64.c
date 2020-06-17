/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abdlb_s64_tied1:
**	sabdlb	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_s64_tied1, svint64_t, svint32_t,
		    z0_res = svabdlb_s64 (z0, z1),
		    z0_res = svabdlb (z0, z1))

/*
** abdlb_s64_tied2:
**	sabdlb	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_s64_tied2, svint64_t, svint32_t,
		    z0_res = svabdlb_s64 (z1, z0),
		    z0_res = svabdlb (z1, z0))

/*
** abdlb_s64_untied:
**	sabdlb	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_s64_untied, svint64_t, svint32_t,
		    z0_res = svabdlb_s64 (z1, z2),
		    z0_res = svabdlb (z1, z2))

/*
** abdlb_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	sabdlb	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlb_w0_s64_tied1, svint64_t, svint32_t, int32_t,
		     z0_res = svabdlb_n_s64 (z0, x0),
		     z0_res = svabdlb (z0, x0))

/*
** abdlb_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	sabdlb	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlb_w0_s64_untied, svint64_t, svint32_t, int32_t,
		     z0_res = svabdlb_n_s64 (z1, x0),
		     z0_res = svabdlb (z1, x0))

/*
** abdlb_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	sabdlb	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_11_s64_tied1, svint64_t, svint32_t,
		    z0_res = svabdlb_n_s64 (z0, 11),
		    z0_res = svabdlb (z0, 11))

/*
** abdlb_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	sabdlb	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_11_s64_untied, svint64_t, svint32_t,
		    z0_res = svabdlb_n_s64 (z1, 11),
		    z0_res = svabdlb (z1, 11))
