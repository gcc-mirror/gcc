/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dupq_0_b64:
**	pfalse	p0\.b
**	ret
*/
TEST_UNIFORM_P (dupq_0_b64,
		p0 = svdupq_n_b64 (0, 0),
		p0 = svdupq_b64 (0, 0))

/*
** dupq_1_b64:
** (
**	ptrue	(p[0-7])\.d, all
**	pfalse	(p[0-7])\.b
**	trn1	p0\.d, \1\.d, \2\.d
** |
**	pfalse	(p[0-7])\.b
**	ptrue	(p[0-7])\.d, all
**	trn1	p0\.d, \4\.d, \3\.d
** )
**	ret
*/
TEST_UNIFORM_P (dupq_1_b64,
		p0 = svdupq_n_b64 (1, 0),
		p0 = svdupq_b64 (1, 0))

/*
** dupq_2_b64:
** (
**	pfalse	(p[0-7])\.b
**	ptrue	(p[0-7])\.d, all
**	trn1	p0\.d, \1\.d, \2\.d
** |
**	ptrue	(p[0-7])\.d, all
**	pfalse	(p[0-7])\.b
**	trn1	p0\.d, \4\.d, \3\.d
** )
**	ret
*/
TEST_UNIFORM_P (dupq_2_b64,
		p0 = svdupq_n_b64 (0, 1),
		p0 = svdupq_b64 (0, 1))

/*
** dupq_3_b64:
**	ptrue	p0\.d, all
**	ret
*/
TEST_UNIFORM_P (dupq_3_b64,
		p0 = svdupq_n_b64 (1, 1),
		p0 = svdupq_b64 (1, 1))
