/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrnt_1_s32:
**	sqshrnt	z0\.h, z4\.s, #1
**	ret
*/
TEST_DUAL_Z (qshrnt_1_s32, svint16_t, svint32_t,
	     z0 = svqshrnt_n_s32 (z0, z4, 1),
	     z0 = svqshrnt (z0, z4, 1))

/*
** qshrnt_2_s32:
**	sqshrnt	z0\.h, z4\.s, #2
**	ret
*/
TEST_DUAL_Z (qshrnt_2_s32, svint16_t, svint32_t,
	     z0 = svqshrnt_n_s32 (z0, z4, 2),
	     z0 = svqshrnt (z0, z4, 2))

/*
** qshrnt_16_s32_tied1:
**	sqshrnt	z0\.h, z4\.s, #16
**	ret
*/
TEST_DUAL_Z (qshrnt_16_s32_tied1, svint16_t, svint32_t,
	     z0 = svqshrnt_n_s32 (z0, z4, 16),
	     z0 = svqshrnt (z0, z4, 16))

/*
** qshrnt_16_s32_untied:
** (
**	mov	z0\.d, z1\.d
**	sqshrnt	z0\.h, z4\.s, #16
** |
**	sqshrnt	z1\.h, z4\.s, #16
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshrnt_16_s32_untied, svint16_t, svint32_t,
	     z0 = svqshrnt_n_s32 (z1, z4, 16),
	     z0 = svqshrnt (z1, z4, 16))
