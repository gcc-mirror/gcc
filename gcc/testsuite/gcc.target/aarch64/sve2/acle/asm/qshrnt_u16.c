/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrnt_1_u16:
**	uqshrnt	z0\.b, z4\.h, #1
**	ret
*/
TEST_DUAL_Z (qshrnt_1_u16, svuint8_t, svuint16_t,
	     z0 = svqshrnt_n_u16 (z0, z4, 1),
	     z0 = svqshrnt (z0, z4, 1))

/*
** qshrnt_2_u16:
**	uqshrnt	z0\.b, z4\.h, #2
**	ret
*/
TEST_DUAL_Z (qshrnt_2_u16, svuint8_t, svuint16_t,
	     z0 = svqshrnt_n_u16 (z0, z4, 2),
	     z0 = svqshrnt (z0, z4, 2))

/*
** qshrnt_8_u16_tied1:
**	uqshrnt	z0\.b, z4\.h, #8
**	ret
*/
TEST_DUAL_Z (qshrnt_8_u16_tied1, svuint8_t, svuint16_t,
	     z0 = svqshrnt_n_u16 (z0, z4, 8),
	     z0 = svqshrnt (z0, z4, 8))

/*
** qshrnt_8_u16_untied:
** (
**	mov	z0\.d, z1\.d
**	uqshrnt	z0\.b, z4\.h, #8
** |
**	uqshrnt	z1\.b, z4\.h, #8
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshrnt_8_u16_untied, svuint8_t, svuint16_t,
	     z0 = svqshrnt_n_u16 (z1, z4, 8),
	     z0 = svqshrnt (z1, z4, 8))
