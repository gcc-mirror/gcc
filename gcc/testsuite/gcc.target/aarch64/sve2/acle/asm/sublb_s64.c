/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sublb_s64_tied1:
**	ssublb	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_s64_tied1, svint64_t, svint32_t,
		    z0_res = svsublb_s64 (z0, z1),
		    z0_res = svsublb (z0, z1))

/*
** sublb_s64_tied2:
**	ssublb	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_s64_tied2, svint64_t, svint32_t,
		    z0_res = svsublb_s64 (z1, z0),
		    z0_res = svsublb (z1, z0))

/*
** sublb_s64_untied:
**	ssublb	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_s64_untied, svint64_t, svint32_t,
		    z0_res = svsublb_s64 (z1, z2),
		    z0_res = svsublb (z1, z2))

/*
** sublb_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	ssublb	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublb_w0_s64_tied1, svint64_t, svint32_t, int32_t,
		     z0_res = svsublb_n_s64 (z0, x0),
		     z0_res = svsublb (z0, x0))

/*
** sublb_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	ssublb	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublb_w0_s64_untied, svint64_t, svint32_t, int32_t,
		     z0_res = svsublb_n_s64 (z1, x0),
		     z0_res = svsublb (z1, x0))

/*
** sublb_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	ssublb	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_11_s64_tied1, svint64_t, svint32_t,
		    z0_res = svsublb_n_s64 (z0, 11),
		    z0_res = svsublb (z0, 11))

/*
** sublb_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	ssublb	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_11_s64_untied, svint64_t, svint32_t,
		    z0_res = svsublb_n_s64 (z1, 11),
		    z0_res = svsublb (z1, 11))
