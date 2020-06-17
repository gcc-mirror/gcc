/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrnt_1_u32:
**	uqshrnt	z0\.h, z4\.s, #1
**	ret
*/
TEST_DUAL_Z (qshrnt_1_u32, svuint16_t, svuint32_t,
	     z0 = svqshrnt_n_u32 (z0, z4, 1),
	     z0 = svqshrnt (z0, z4, 1))

/*
** qshrnt_2_u32:
**	uqshrnt	z0\.h, z4\.s, #2
**	ret
*/
TEST_DUAL_Z (qshrnt_2_u32, svuint16_t, svuint32_t,
	     z0 = svqshrnt_n_u32 (z0, z4, 2),
	     z0 = svqshrnt (z0, z4, 2))

/*
** qshrnt_16_u32_tied1:
**	uqshrnt	z0\.h, z4\.s, #16
**	ret
*/
TEST_DUAL_Z (qshrnt_16_u32_tied1, svuint16_t, svuint32_t,
	     z0 = svqshrnt_n_u32 (z0, z4, 16),
	     z0 = svqshrnt (z0, z4, 16))

/*
** qshrnt_16_u32_untied:
** (
**	mov	z0\.d, z1\.d
**	uqshrnt	z0\.h, z4\.s, #16
** |
**	uqshrnt	z1\.h, z4\.s, #16
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshrnt_16_u32_untied, svuint16_t, svuint32_t,
	     z0 = svqshrnt_n_u32 (z1, z4, 16),
	     z0 = svqshrnt (z1, z4, 16))
