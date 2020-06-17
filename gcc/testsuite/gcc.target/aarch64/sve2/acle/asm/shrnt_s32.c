/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shrnt_1_s32:
**	shrnt	z0\.h, z4\.s, #1
**	ret
*/
TEST_DUAL_Z (shrnt_1_s32, svint16_t, svint32_t,
	     z0 = svshrnt_n_s32 (z0, z4, 1),
	     z0 = svshrnt (z0, z4, 1))

/*
** shrnt_2_s32:
**	shrnt	z0\.h, z4\.s, #2
**	ret
*/
TEST_DUAL_Z (shrnt_2_s32, svint16_t, svint32_t,
	     z0 = svshrnt_n_s32 (z0, z4, 2),
	     z0 = svshrnt (z0, z4, 2))

/*
** shrnt_16_s32_tied1:
**	shrnt	z0\.h, z4\.s, #16
**	ret
*/
TEST_DUAL_Z (shrnt_16_s32_tied1, svint16_t, svint32_t,
	     z0 = svshrnt_n_s32 (z0, z4, 16),
	     z0 = svshrnt (z0, z4, 16))

/*
** shrnt_16_s32_untied:
** (
**	mov	z0\.d, z1\.d
**	shrnt	z0\.h, z4\.s, #16
** |
**	shrnt	z1\.h, z4\.s, #16
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (shrnt_16_s32_untied, svint16_t, svint32_t,
	     z0 = svshrnt_n_s32 (z1, z4, 16),
	     z0 = svshrnt (z1, z4, 16))
