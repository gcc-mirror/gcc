/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrnt_1_s16:
**	sqrshrnt	z0\.b, z4\.h, #1
**	ret
*/
TEST_DUAL_Z (qrshrnt_1_s16, svint8_t, svint16_t,
	     z0 = svqrshrnt_n_s16 (z0, z4, 1),
	     z0 = svqrshrnt (z0, z4, 1))

/*
** qrshrnt_2_s16:
**	sqrshrnt	z0\.b, z4\.h, #2
**	ret
*/
TEST_DUAL_Z (qrshrnt_2_s16, svint8_t, svint16_t,
	     z0 = svqrshrnt_n_s16 (z0, z4, 2),
	     z0 = svqrshrnt (z0, z4, 2))

/*
** qrshrnt_8_s16_tied1:
**	sqrshrnt	z0\.b, z4\.h, #8
**	ret
*/
TEST_DUAL_Z (qrshrnt_8_s16_tied1, svint8_t, svint16_t,
	     z0 = svqrshrnt_n_s16 (z0, z4, 8),
	     z0 = svqrshrnt (z0, z4, 8))

/*
** qrshrnt_8_s16_untied:
** (
**	mov	z0\.d, z1\.d
**	sqrshrnt	z0\.b, z4\.h, #8
** |
**	sqrshrnt	z1\.b, z4\.h, #8
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshrnt_8_s16_untied, svint8_t, svint16_t,
	     z0 = svqrshrnt_n_s16 (z1, z4, 8),
	     z0 = svqrshrnt (z1, z4, 8))
