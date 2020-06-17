/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrnt_1_s32:
**	sqrshrnt	z0\.h, z4\.s, #1
**	ret
*/
TEST_DUAL_Z (qrshrnt_1_s32, svint16_t, svint32_t,
	     z0 = svqrshrnt_n_s32 (z0, z4, 1),
	     z0 = svqrshrnt (z0, z4, 1))

/*
** qrshrnt_2_s32:
**	sqrshrnt	z0\.h, z4\.s, #2
**	ret
*/
TEST_DUAL_Z (qrshrnt_2_s32, svint16_t, svint32_t,
	     z0 = svqrshrnt_n_s32 (z0, z4, 2),
	     z0 = svqrshrnt (z0, z4, 2))

/*
** qrshrnt_16_s32_tied1:
**	sqrshrnt	z0\.h, z4\.s, #16
**	ret
*/
TEST_DUAL_Z (qrshrnt_16_s32_tied1, svint16_t, svint32_t,
	     z0 = svqrshrnt_n_s32 (z0, z4, 16),
	     z0 = svqrshrnt (z0, z4, 16))

/*
** qrshrnt_16_s32_untied:
** (
**	mov	z0\.d, z1\.d
**	sqrshrnt	z0\.h, z4\.s, #16
** |
**	sqrshrnt	z1\.h, z4\.s, #16
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshrnt_16_s32_untied, svint16_t, svint32_t,
	     z0 = svqrshrnt_n_s32 (z1, z4, 16),
	     z0 = svqrshrnt (z1, z4, 16))
