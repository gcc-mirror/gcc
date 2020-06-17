/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrnt_1_s16:
**	sqshrnt	z0\.b, z4\.h, #1
**	ret
*/
TEST_DUAL_Z (qshrnt_1_s16, svint8_t, svint16_t,
	     z0 = svqshrnt_n_s16 (z0, z4, 1),
	     z0 = svqshrnt (z0, z4, 1))

/*
** qshrnt_2_s16:
**	sqshrnt	z0\.b, z4\.h, #2
**	ret
*/
TEST_DUAL_Z (qshrnt_2_s16, svint8_t, svint16_t,
	     z0 = svqshrnt_n_s16 (z0, z4, 2),
	     z0 = svqshrnt (z0, z4, 2))

/*
** qshrnt_8_s16_tied1:
**	sqshrnt	z0\.b, z4\.h, #8
**	ret
*/
TEST_DUAL_Z (qshrnt_8_s16_tied1, svint8_t, svint16_t,
	     z0 = svqshrnt_n_s16 (z0, z4, 8),
	     z0 = svqshrnt (z0, z4, 8))

/*
** qshrnt_8_s16_untied:
** (
**	mov	z0\.d, z1\.d
**	sqshrnt	z0\.b, z4\.h, #8
** |
**	sqshrnt	z1\.b, z4\.h, #8
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshrnt_8_s16_untied, svint8_t, svint16_t,
	     z0 = svqshrnt_n_s16 (z1, z4, 8),
	     z0 = svqshrnt (z1, z4, 8))
