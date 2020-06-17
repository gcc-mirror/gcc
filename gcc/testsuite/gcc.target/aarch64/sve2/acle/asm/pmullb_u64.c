/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** pmullb_u64_tied1:
**	pmullb	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullb_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svpmullb_u64 (z0, z1),
		    z0_res = svpmullb (z0, z1))

/*
** pmullb_u64_tied2:
**	pmullb	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullb_u64_tied2, svuint64_t, svuint32_t,
		    z0_res = svpmullb_u64 (z1, z0),
		    z0_res = svpmullb (z1, z0))

/*
** pmullb_u64_untied:
**	pmullb	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullb_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svpmullb_u64 (z1, z2),
		    z0_res = svpmullb (z1, z2))

/*
** pmullb_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	pmullb	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (pmullb_w0_u64_tied1, svuint64_t, svuint32_t, uint32_t,
		     z0_res = svpmullb_n_u64 (z0, x0),
		     z0_res = svpmullb (z0, x0))

/*
** pmullb_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	pmullb	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (pmullb_w0_u64_untied, svuint64_t, svuint32_t, uint32_t,
		     z0_res = svpmullb_n_u64 (z1, x0),
		     z0_res = svpmullb (z1, x0))

/*
** pmullb_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	pmullb	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullb_11_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svpmullb_n_u64 (z0, 11),
		    z0_res = svpmullb (z0, 11))

/*
** pmullb_11_u64_untied:
**	mov	(z[0-9]+\.s), #11
**	pmullb	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullb_11_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svpmullb_n_u64 (z1, 11),
		    z0_res = svpmullb (z1, 11))
