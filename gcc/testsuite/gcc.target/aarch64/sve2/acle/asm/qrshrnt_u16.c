/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrnt_1_u16:
**	uqrshrnt	z0\.b, z4\.h, #1
**	ret
*/
TEST_DUAL_Z (qrshrnt_1_u16, svuint8_t, svuint16_t,
	     z0 = svqrshrnt_n_u16 (z0, z4, 1),
	     z0 = svqrshrnt (z0, z4, 1))

/*
** qrshrnt_2_u16:
**	uqrshrnt	z0\.b, z4\.h, #2
**	ret
*/
TEST_DUAL_Z (qrshrnt_2_u16, svuint8_t, svuint16_t,
	     z0 = svqrshrnt_n_u16 (z0, z4, 2),
	     z0 = svqrshrnt (z0, z4, 2))

/*
** qrshrnt_8_u16_tied1:
**	uqrshrnt	z0\.b, z4\.h, #8
**	ret
*/
TEST_DUAL_Z (qrshrnt_8_u16_tied1, svuint8_t, svuint16_t,
	     z0 = svqrshrnt_n_u16 (z0, z4, 8),
	     z0 = svqrshrnt (z0, z4, 8))

/*
** qrshrnt_8_u16_untied:
** (
**	mov	z0\.d, z1\.d
**	uqrshrnt	z0\.b, z4\.h, #8
** |
**	uqrshrnt	z1\.b, z4\.h, #8
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshrnt_8_u16_untied, svuint8_t, svuint16_t,
	     z0 = svqrshrnt_n_u16 (z1, z4, 8),
	     z0 = svqrshrnt (z1, z4, 8))
