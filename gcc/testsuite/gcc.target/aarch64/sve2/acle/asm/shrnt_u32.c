/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shrnt_1_u32:
**	shrnt	z0\.h, z4\.s, #1
**	ret
*/
TEST_DUAL_Z (shrnt_1_u32, svuint16_t, svuint32_t,
	     z0 = svshrnt_n_u32 (z0, z4, 1),
	     z0 = svshrnt (z0, z4, 1))

/*
** shrnt_2_u32:
**	shrnt	z0\.h, z4\.s, #2
**	ret
*/
TEST_DUAL_Z (shrnt_2_u32, svuint16_t, svuint32_t,
	     z0 = svshrnt_n_u32 (z0, z4, 2),
	     z0 = svshrnt (z0, z4, 2))

/*
** shrnt_16_u32_tied1:
**	shrnt	z0\.h, z4\.s, #16
**	ret
*/
TEST_DUAL_Z (shrnt_16_u32_tied1, svuint16_t, svuint32_t,
	     z0 = svshrnt_n_u32 (z0, z4, 16),
	     z0 = svshrnt (z0, z4, 16))

/*
** shrnt_16_u32_untied:
** (
**	mov	z0\.d, z1\.d
**	shrnt	z0\.h, z4\.s, #16
** |
**	shrnt	z1\.h, z4\.s, #16
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (shrnt_16_u32_untied, svuint16_t, svuint32_t,
	     z0 = svshrnt_n_u32 (z1, z4, 16),
	     z0 = svshrnt (z1, z4, 16))
