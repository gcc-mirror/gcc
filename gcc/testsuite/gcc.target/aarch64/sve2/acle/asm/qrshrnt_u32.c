/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrnt_1_u32:
**	uqrshrnt	z0\.h, z4\.s, #1
**	ret
*/
TEST_DUAL_Z (qrshrnt_1_u32, svuint16_t, svuint32_t,
	     z0 = svqrshrnt_n_u32 (z0, z4, 1),
	     z0 = svqrshrnt (z0, z4, 1))

/*
** qrshrnt_2_u32:
**	uqrshrnt	z0\.h, z4\.s, #2
**	ret
*/
TEST_DUAL_Z (qrshrnt_2_u32, svuint16_t, svuint32_t,
	     z0 = svqrshrnt_n_u32 (z0, z4, 2),
	     z0 = svqrshrnt (z0, z4, 2))

/*
** qrshrnt_16_u32_tied1:
**	uqrshrnt	z0\.h, z4\.s, #16
**	ret
*/
TEST_DUAL_Z (qrshrnt_16_u32_tied1, svuint16_t, svuint32_t,
	     z0 = svqrshrnt_n_u32 (z0, z4, 16),
	     z0 = svqrshrnt (z0, z4, 16))

/*
** qrshrnt_16_u32_untied:
** (
**	mov	z0\.d, z1\.d
**	uqrshrnt	z0\.h, z4\.s, #16
** |
**	uqrshrnt	z1\.h, z4\.s, #16
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshrnt_16_u32_untied, svuint16_t, svuint32_t,
	     z0 = svqrshrnt_n_u32 (z1, z4, 16),
	     z0 = svqrshrnt (z1, z4, 16))
