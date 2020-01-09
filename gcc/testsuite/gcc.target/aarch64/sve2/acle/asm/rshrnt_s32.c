/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshrnt_1_s32:
**	rshrnt	z0\.h, z4\.s, #1
**	ret
*/
TEST_DUAL_Z (rshrnt_1_s32, svint16_t, svint32_t,
	     z0 = svrshrnt_n_s32 (z0, z4, 1),
	     z0 = svrshrnt (z0, z4, 1))

/*
** rshrnt_2_s32:
**	rshrnt	z0\.h, z4\.s, #2
**	ret
*/
TEST_DUAL_Z (rshrnt_2_s32, svint16_t, svint32_t,
	     z0 = svrshrnt_n_s32 (z0, z4, 2),
	     z0 = svrshrnt (z0, z4, 2))

/*
** rshrnt_16_s32_tied1:
**	rshrnt	z0\.h, z4\.s, #16
**	ret
*/
TEST_DUAL_Z (rshrnt_16_s32_tied1, svint16_t, svint32_t,
	     z0 = svrshrnt_n_s32 (z0, z4, 16),
	     z0 = svrshrnt (z0, z4, 16))

/*
** rshrnt_16_s32_untied:
** (
**	mov	z0\.d, z1\.d
**	rshrnt	z0\.h, z4\.s, #16
** |
**	rshrnt	z1\.h, z4\.s, #16
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (rshrnt_16_s32_untied, svint16_t, svint32_t,
	     z0 = svrshrnt_n_s32 (z1, z4, 16),
	     z0 = svrshrnt (z1, z4, 16))
