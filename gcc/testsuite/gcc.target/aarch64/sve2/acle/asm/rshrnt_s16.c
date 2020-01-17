/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshrnt_1_s16:
**	rshrnt	z0\.b, z4\.h, #1
**	ret
*/
TEST_DUAL_Z (rshrnt_1_s16, svint8_t, svint16_t,
	     z0 = svrshrnt_n_s16 (z0, z4, 1),
	     z0 = svrshrnt (z0, z4, 1))

/*
** rshrnt_2_s16:
**	rshrnt	z0\.b, z4\.h, #2
**	ret
*/
TEST_DUAL_Z (rshrnt_2_s16, svint8_t, svint16_t,
	     z0 = svrshrnt_n_s16 (z0, z4, 2),
	     z0 = svrshrnt (z0, z4, 2))

/*
** rshrnt_8_s16_tied1:
**	rshrnt	z0\.b, z4\.h, #8
**	ret
*/
TEST_DUAL_Z (rshrnt_8_s16_tied1, svint8_t, svint16_t,
	     z0 = svrshrnt_n_s16 (z0, z4, 8),
	     z0 = svrshrnt (z0, z4, 8))

/*
** rshrnt_8_s16_untied:
** (
**	mov	z0\.d, z1\.d
**	rshrnt	z0\.b, z4\.h, #8
** |
**	rshrnt	z1\.b, z4\.h, #8
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (rshrnt_8_s16_untied, svint8_t, svint16_t,
	     z0 = svrshrnt_n_s16 (z1, z4, 8),
	     z0 = svrshrnt (z1, z4, 8))
