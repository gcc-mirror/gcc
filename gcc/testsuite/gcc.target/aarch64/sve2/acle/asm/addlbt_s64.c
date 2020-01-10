/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addlbt_s64_tied1:
**	saddlbt	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_s64_tied1, svint64_t, svint32_t,
		    z0_res = svaddlbt_s64 (z0, z1),
		    z0_res = svaddlbt (z0, z1))

/*
** addlbt_s64_tied2:
**	saddlbt	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_s64_tied2, svint64_t, svint32_t,
		    z0_res = svaddlbt_s64 (z1, z0),
		    z0_res = svaddlbt (z1, z0))

/*
** addlbt_s64_untied:
**	saddlbt	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_s64_untied, svint64_t, svint32_t,
		    z0_res = svaddlbt_s64 (z1, z2),
		    z0_res = svaddlbt (z1, z2))

/*
** addlbt_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	saddlbt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlbt_w0_s64_tied1, svint64_t, svint32_t, int32_t,
		     z0_res = svaddlbt_n_s64 (z0, x0),
		     z0_res = svaddlbt (z0, x0))

/*
** addlbt_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	saddlbt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlbt_w0_s64_untied, svint64_t, svint32_t, int32_t,
		     z0_res = svaddlbt_n_s64 (z1, x0),
		     z0_res = svaddlbt (z1, x0))

/*
** addlbt_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	saddlbt	z0\.d, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_11_s64_tied1, svint64_t, svint32_t,
		    z0_res = svaddlbt_n_s64 (z0, 11),
		    z0_res = svaddlbt (z0, 11))

/*
** addlbt_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	saddlbt	z0\.d, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_11_s64_untied, svint64_t, svint32_t,
		    z0_res = svaddlbt_n_s64 (z1, 11),
		    z0_res = svaddlbt (z1, 11))
