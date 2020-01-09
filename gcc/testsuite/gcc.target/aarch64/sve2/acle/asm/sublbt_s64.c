/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sublbt_s64_tied1:
**	ssublbt	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_s64_tied1, svint64_t, svint32_t,
		    z0_res = svsublbt_s64 (z0, z1),
		    z0_res = svsublbt (z0, z1))

/*
** sublbt_s64_tied2:
**	ssublbt	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_s64_tied2, svint64_t, svint32_t,
		    z0_res = svsublbt_s64 (z1, z0),
		    z0_res = svsublbt (z1, z0))

/*
** sublbt_s64_untied:
**	ssublbt	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_s64_untied, svint64_t, svint32_t,
		    z0_res = svsublbt_s64 (z1, z2),
		    z0_res = svsublbt (z1, z2))

/*
** sublbt_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	ssublbt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublbt_w0_s64_tied1, svint64_t, svint32_t, int32_t,
		     z0_res = svsublbt_n_s64 (z0, x0),
		     z0_res = svsublbt (z0, x0))

/*
** sublbt_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	ssublbt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublbt_w0_s64_untied, svint64_t, svint32_t, int32_t,
		     z0_res = svsublbt_n_s64 (z1, x0),
		     z0_res = svsublbt (z1, x0))

/*
** sublbt_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	ssublbt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_11_s64_tied1, svint64_t, svint32_t,
		    z0_res = svsublbt_n_s64 (z0, 11),
		    z0_res = svsublbt (z0, 11))

/*
** sublbt_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	ssublbt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_11_s64_untied, svint64_t, svint32_t,
		    z0_res = svsublbt_n_s64 (z1, 11),
		    z0_res = svsublbt (z1, 11))
