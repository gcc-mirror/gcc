/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qinch_1_s16_tied:
**	sqinch	z0\.h
**	ret
*/
TEST_UNIFORM_Z (qinch_1_s16_tied, svint16_t,
		z0 = svqinch_s16 (z0, 1),
		z0 = svqinch (z0, 1))

/*
** qinch_1_s16_untied:
**	movprfx	z0, z1
**	sqinch	z0\.h
**	ret
*/
TEST_UNIFORM_Z (qinch_1_s16_untied, svint16_t,
		z0 = svqinch_s16 (z1, 1),
		z0 = svqinch (z1, 1))

/*
** qinch_2_s16:
**	sqinch	z0\.h, all, mul #2
**	ret
*/
TEST_UNIFORM_Z (qinch_2_s16, svint16_t,
		z0 = svqinch_s16 (z0, 2),
		z0 = svqinch (z0, 2))

/*
** qinch_7_s16:
**	sqinch	z0\.h, all, mul #7
**	ret
*/
TEST_UNIFORM_Z (qinch_7_s16, svint16_t,
		z0 = svqinch_s16 (z0, 7),
		z0 = svqinch (z0, 7))

/*
** qinch_15_s16:
**	sqinch	z0\.h, all, mul #15
**	ret
*/
TEST_UNIFORM_Z (qinch_15_s16, svint16_t,
		z0 = svqinch_s16 (z0, 15),
		z0 = svqinch (z0, 15))

/*
** qinch_16_s16:
**	sqinch	z0\.h, all, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_16_s16, svint16_t,
		z0 = svqinch_s16 (z0, 16),
		z0 = svqinch (z0, 16))
