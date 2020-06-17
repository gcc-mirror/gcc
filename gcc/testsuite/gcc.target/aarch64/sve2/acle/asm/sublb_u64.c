/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sublb_u64_tied1:
**	usublb	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svsublb_u64 (z0, z1),
		    z0_res = svsublb (z0, z1))

/*
** sublb_u64_tied2:
**	usublb	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_u64_tied2, svuint64_t, svuint32_t,
		    z0_res = svsublb_u64 (z1, z0),
		    z0_res = svsublb (z1, z0))

/*
** sublb_u64_untied:
**	usublb	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svsublb_u64 (z1, z2),
		    z0_res = svsublb (z1, z2))

/*
** sublb_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	usublb	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublb_w0_u64_tied1, svuint64_t, svuint32_t, uint32_t,
		     z0_res = svsublb_n_u64 (z0, x0),
		     z0_res = svsublb (z0, x0))

/*
** sublb_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	usublb	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublb_w0_u64_untied, svuint64_t, svuint32_t, uint32_t,
		     z0_res = svsublb_n_u64 (z1, x0),
		     z0_res = svsublb (z1, x0))

/*
** sublb_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	usublb	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_11_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svsublb_n_u64 (z0, 11),
		    z0_res = svsublb (z0, 11))

/*
** sublb_11_u64_untied:
**	mov	(z[0-9]+\.s), #11
**	usublb	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_11_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svsublb_n_u64 (z1, 11),
		    z0_res = svsublb (z1, 11))
