/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dupq_0_b32:
**	pfalse	p0\.b
**	ret
*/
TEST_UNIFORM_P (dupq_0_b32,
		p0 = svdupq_n_b32 (0, 0, 0, 0),
		p0 = svdupq_b32 (0, 0, 0, 0))

/*
** dupq_1_b32:
** (
**	ptrue	(p[0-9]+)\.d, all
**	pfalse	(p[0-9]+)\.b
**	trn1	p0\.d, \1\.d, \2\.d
** |
**	pfalse	(p[0-9]+)\.b
**	ptrue	(p[0-9]+)\.d, all
**	trn1	p0\.d, \4\.d, \3\.d
** )
**	ret
*/
TEST_UNIFORM_P (dupq_1_b32,
		p0 = svdupq_n_b32 (1, 0, 0, 0),
		p0 = svdupq_b32 (1, 0, 0, 0))

/*
** dupq_3_b32:
** (
**	ptrue	(p[0-9]+)\.s, all
**	pfalse	(p[0-9]+)\.b
**	trn1	p0\.d, \1\.d, \2\.d
** |
**	pfalse	(p[0-9]+)\.b
**	ptrue	(p[0-9]+)\.s, all
**	trn1	p0\.d, \4\.d, \3\.d
** )
**	ret
*/
TEST_UNIFORM_P (dupq_3_b32,
		p0 = svdupq_n_b32 (1, 1, 0, 0),
		p0 = svdupq_b32 (1, 1, 0, 0))

/*
** dupq_4_b32:
** (
**	pfalse	(p[0-9]+)\.b
**	ptrue	(p[0-9]+)\.d, all
**	trn1	p0\.d, \1\.d, \2\.d
** |
**	ptrue	(p[0-9]+)\.d, all
**	pfalse	(p[0-9]+)\.b
**	trn1	p0\.d, \4\.d, \3\.d
** )
**	ret
*/
TEST_UNIFORM_P (dupq_4_b32,
		p0 = svdupq_n_b32 (0, 0, 1, 0),
		p0 = svdupq_b32 (0, 0, 1, 0))

/*
** dupq_5_b32:
**	ptrue	p0\.d, all
**	ret
*/
TEST_UNIFORM_P (dupq_5_b32,
		p0 = svdupq_n_b32 (1, 0, 1, 0),
		p0 = svdupq_b32 (1, 0, 1, 0))

/*
** dupq_7_b32:
** (
**	ptrue	(p[0-9]+)\.s, all
**	ptrue	(p[0-9]+)\.d, all
**	trn1	p0\.d, \1\.d, \2\.d
** |
**	ptrue	(p[0-9]+)\.d, all
**	ptrue	(p[0-9]+)\.s, all
**	trn1	p0\.d, \4\.d, \3\.d
** )
**	ret
*/
TEST_UNIFORM_P (dupq_7_b32,
		p0 = svdupq_n_b32 (1, 1, 1, 0),
		p0 = svdupq_b32 (1, 1, 1, 0))

/*
** dupq_a_b32:
** (
**	ptrue	(p[0-9]+)\.d, all
**	ptrue	(p[0-9]+)\.s, all
**	not	p0\.b, \2/z, \1\.b
** |
**	ptrue	(p[0-9]+)\.s, all
**	ptrue	(p[0-9]+)\.d, all
**	not	p0\.b, \3/z, \4\.b
** )
**	ret
*/
TEST_UNIFORM_P (dupq_a_b32,
		p0 = svdupq_n_b32 (0, 1, 0, 1),
		p0 = svdupq_b32 (0, 1, 0, 1))

/*
** dupq_e_b32:
** (
**	ptrue	(p[0-9]+)\.d, all
**	ptrue	(p[0-9]+)\.s, all
**	trn1	p0\.d, \1\.d, \2\.d
** |
**	ptrue	(p[0-9]+)\.s, all
**	ptrue	(p[0-9]+)\.d, all
**	trn1	p0\.d, \4\.d, \3\.d
** )
**	ret
*/
TEST_UNIFORM_P (dupq_e_b32,
		p0 = svdupq_n_b32 (1, 0, 1, 1),
		p0 = svdupq_b32 (1, 0, 1, 1))

/*
** dupq_f_b32:
**	ptrue	p0\.s, all
**	ret
*/
TEST_UNIFORM_P (dupq_f_b32,
		p0 = svdupq_n_b32 (1, 1, 1, 1),
		p0 = svdupq_b32 (1, 1, 1, 1))
