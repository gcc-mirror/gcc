/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrunt_1_s32:
**	sqrshrunt	z0\.h, z4\.s, #1
**	ret
*/
TEST_DUAL_Z (qrshrunt_1_s32, svuint16_t, svint32_t,
	     z0 = svqrshrunt_n_s32 (z0, z4, 1),
	     z0 = svqrshrunt (z0, z4, 1))

/*
** qrshrunt_2_s32:
**	sqrshrunt	z0\.h, z4\.s, #2
**	ret
*/
TEST_DUAL_Z (qrshrunt_2_s32, svuint16_t, svint32_t,
	     z0 = svqrshrunt_n_s32 (z0, z4, 2),
	     z0 = svqrshrunt (z0, z4, 2))

/*
** qrshrunt_16_s32_tied1:
**	sqrshrunt	z0\.h, z4\.s, #16
**	ret
*/
TEST_DUAL_Z (qrshrunt_16_s32_tied1, svuint16_t, svint32_t,
	     z0 = svqrshrunt_n_s32 (z0, z4, 16),
	     z0 = svqrshrunt (z0, z4, 16))

/*
** qrshrunt_16_s32_untied:
** (
**	mov	z0\.d, z1\.d
**	sqrshrunt	z0\.h, z4\.s, #16
** |
**	sqrshrunt	z1\.h, z4\.s, #16
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshrunt_16_s32_untied, svuint16_t, svint32_t,
	     z0 = svqrshrunt_n_s32 (z1, z4, 16),
	     z0 = svqrshrunt (z1, z4, 16))
