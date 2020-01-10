/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrunt_1_s16:
**	sqrshrunt	z0\.b, z4\.h, #1
**	ret
*/
TEST_DUAL_Z (qrshrunt_1_s16, svuint8_t, svint16_t,
	     z0 = svqrshrunt_n_s16 (z0, z4, 1),
	     z0 = svqrshrunt (z0, z4, 1))

/*
** qrshrunt_2_s16:
**	sqrshrunt	z0\.b, z4\.h, #2
**	ret
*/
TEST_DUAL_Z (qrshrunt_2_s16, svuint8_t, svint16_t,
	     z0 = svqrshrunt_n_s16 (z0, z4, 2),
	     z0 = svqrshrunt (z0, z4, 2))

/*
** qrshrunt_8_s16_tied1:
**	sqrshrunt	z0\.b, z4\.h, #8
**	ret
*/
TEST_DUAL_Z (qrshrunt_8_s16_tied1, svuint8_t, svint16_t,
	     z0 = svqrshrunt_n_s16 (z0, z4, 8),
	     z0 = svqrshrunt (z0, z4, 8))

/*
** qrshrunt_8_s16_untied:
** (
**	mov	z0\.d, z1\.d
**	sqrshrunt	z0\.b, z4\.h, #8
** |
**	sqrshrunt	z1\.b, z4\.h, #8
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshrunt_8_s16_untied, svuint8_t, svint16_t,
	     z0 = svqrshrunt_n_s16 (z1, z4, 8),
	     z0 = svqrshrunt (z1, z4, 8))
