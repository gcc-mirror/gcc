/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrunt_1_s16:
**	sqshrunt	z0\.b, z4\.h, #1
**	ret
*/
TEST_DUAL_Z (qshrunt_1_s16, svuint8_t, svint16_t,
	     z0 = svqshrunt_n_s16 (z0, z4, 1),
	     z0 = svqshrunt (z0, z4, 1))

/*
** qshrunt_2_s16:
**	sqshrunt	z0\.b, z4\.h, #2
**	ret
*/
TEST_DUAL_Z (qshrunt_2_s16, svuint8_t, svint16_t,
	     z0 = svqshrunt_n_s16 (z0, z4, 2),
	     z0 = svqshrunt (z0, z4, 2))

/*
** qshrunt_8_s16_tied1:
**	sqshrunt	z0\.b, z4\.h, #8
**	ret
*/
TEST_DUAL_Z (qshrunt_8_s16_tied1, svuint8_t, svint16_t,
	     z0 = svqshrunt_n_s16 (z0, z4, 8),
	     z0 = svqshrunt (z0, z4, 8))

/*
** qshrunt_8_s16_untied:
** (
**	mov	z0\.d, z1\.d
**	sqshrunt	z0\.b, z4\.h, #8
** |
**	sqshrunt	z1\.b, z4\.h, #8
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshrunt_8_s16_untied, svuint8_t, svint16_t,
	     z0 = svqshrunt_n_s16 (z1, z4, 8),
	     z0 = svqshrunt (z1, z4, 8))
