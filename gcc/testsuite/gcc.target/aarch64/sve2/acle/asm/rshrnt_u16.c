/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshrnt_1_u16:
**	rshrnt	z0\.b, z4\.h, #1
**	ret
*/
TEST_DUAL_Z (rshrnt_1_u16, svuint8_t, svuint16_t,
	     z0 = svrshrnt_n_u16 (z0, z4, 1),
	     z0 = svrshrnt (z0, z4, 1))

/*
** rshrnt_2_u16:
**	rshrnt	z0\.b, z4\.h, #2
**	ret
*/
TEST_DUAL_Z (rshrnt_2_u16, svuint8_t, svuint16_t,
	     z0 = svrshrnt_n_u16 (z0, z4, 2),
	     z0 = svrshrnt (z0, z4, 2))

/*
** rshrnt_8_u16_tied1:
**	rshrnt	z0\.b, z4\.h, #8
**	ret
*/
TEST_DUAL_Z (rshrnt_8_u16_tied1, svuint8_t, svuint16_t,
	     z0 = svrshrnt_n_u16 (z0, z4, 8),
	     z0 = svrshrnt (z0, z4, 8))

/*
** rshrnt_8_u16_untied:
** (
**	mov	z0\.d, z1\.d
**	rshrnt	z0\.b, z4\.h, #8
** |
**	rshrnt	z1\.b, z4\.h, #8
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (rshrnt_8_u16_untied, svuint8_t, svuint16_t,
	     z0 = svrshrnt_n_u16 (z1, z4, 8),
	     z0 = svrshrnt (z1, z4, 8))
