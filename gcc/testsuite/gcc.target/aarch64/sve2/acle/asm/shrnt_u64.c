/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shrnt_1_u64:
**	shrnt	z0\.s, z4\.d, #1
**	ret
*/
TEST_DUAL_Z (shrnt_1_u64, svuint32_t, svuint64_t,
	     z0 = svshrnt_n_u64 (z0, z4, 1),
	     z0 = svshrnt (z0, z4, 1))

/*
** shrnt_2_u64:
**	shrnt	z0\.s, z4\.d, #2
**	ret
*/
TEST_DUAL_Z (shrnt_2_u64, svuint32_t, svuint64_t,
	     z0 = svshrnt_n_u64 (z0, z4, 2),
	     z0 = svshrnt (z0, z4, 2))

/*
** shrnt_32_u64_tied1:
**	shrnt	z0\.s, z4\.d, #32
**	ret
*/
TEST_DUAL_Z (shrnt_32_u64_tied1, svuint32_t, svuint64_t,
	     z0 = svshrnt_n_u64 (z0, z4, 32),
	     z0 = svshrnt (z0, z4, 32))

/*
** shrnt_32_u64_untied:
** (
**	mov	z0\.d, z1\.d
**	shrnt	z0\.s, z4\.d, #32
** |
**	shrnt	z1\.s, z4\.d, #32
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (shrnt_32_u64_untied, svuint32_t, svuint64_t,
	     z0 = svshrnt_n_u64 (z1, z4, 32),
	     z0 = svshrnt (z1, z4, 32))
