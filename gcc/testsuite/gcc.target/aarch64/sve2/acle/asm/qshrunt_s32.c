/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrunt_1_s32:
**	sqshrunt	z0\.h, z4\.s, #1
**	ret
*/
TEST_DUAL_Z (qshrunt_1_s32, svuint16_t, svint32_t,
	     z0 = svqshrunt_n_s32 (z0, z4, 1),
	     z0 = svqshrunt (z0, z4, 1))

/*
** qshrunt_2_s32:
**	sqshrunt	z0\.h, z4\.s, #2
**	ret
*/
TEST_DUAL_Z (qshrunt_2_s32, svuint16_t, svint32_t,
	     z0 = svqshrunt_n_s32 (z0, z4, 2),
	     z0 = svqshrunt (z0, z4, 2))

/*
** qshrunt_16_s32_tied1:
**	sqshrunt	z0\.h, z4\.s, #16
**	ret
*/
TEST_DUAL_Z (qshrunt_16_s32_tied1, svuint16_t, svint32_t,
	     z0 = svqshrunt_n_s32 (z0, z4, 16),
	     z0 = svqshrunt (z0, z4, 16))

/*
** qshrunt_16_s32_untied:
** (
**	mov	z0\.d, z1\.d
**	sqshrunt	z0\.h, z4\.s, #16
** |
**	sqshrunt	z1\.h, z4\.s, #16
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshrunt_16_s32_untied, svuint16_t, svint32_t,
	     z0 = svqshrunt_n_s32 (z1, z4, 16),
	     z0 = svqshrunt (z1, z4, 16))
