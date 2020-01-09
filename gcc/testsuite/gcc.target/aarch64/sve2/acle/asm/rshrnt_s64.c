/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshrnt_1_s64:
**	rshrnt	z0\.s, z4\.d, #1
**	ret
*/
TEST_DUAL_Z (rshrnt_1_s64, svint32_t, svint64_t,
	     z0 = svrshrnt_n_s64 (z0, z4, 1),
	     z0 = svrshrnt (z0, z4, 1))

/*
** rshrnt_2_s64:
**	rshrnt	z0\.s, z4\.d, #2
**	ret
*/
TEST_DUAL_Z (rshrnt_2_s64, svint32_t, svint64_t,
	     z0 = svrshrnt_n_s64 (z0, z4, 2),
	     z0 = svrshrnt (z0, z4, 2))

/*
** rshrnt_32_s64_tied1:
**	rshrnt	z0\.s, z4\.d, #32
**	ret
*/
TEST_DUAL_Z (rshrnt_32_s64_tied1, svint32_t, svint64_t,
	     z0 = svrshrnt_n_s64 (z0, z4, 32),
	     z0 = svrshrnt (z0, z4, 32))

/*
** rshrnt_32_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	rshrnt	z0\.s, z4\.d, #32
** |
**	rshrnt	z1\.s, z4\.d, #32
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (rshrnt_32_s64_untied, svint32_t, svint64_t,
	     z0 = svrshrnt_n_s64 (z1, z4, 32),
	     z0 = svrshrnt (z1, z4, 32))
