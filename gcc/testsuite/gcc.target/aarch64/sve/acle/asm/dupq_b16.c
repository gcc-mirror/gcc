/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dupq_00_b16:
**	pfalse	p0\.b
**	ret
*/
TEST_UNIFORM_P (dupq_00_b16,
		p0 = svdupq_n_b16 (0, 0, 0, 0, 0, 0, 0, 0),
		p0 = svdupq_b16 (0, 0, 0, 0, 0, 0, 0, 0))

/*
** dupq_11_b16:
**	ptrue	p0\.d, all
**	ret
*/
TEST_UNIFORM_P (dupq_11_b16,
		p0 = svdupq_n_b16 (1, 0, 0, 0, 1, 0, 0, 0),
		p0 = svdupq_b16 (1, 0, 0, 0, 1, 0, 0, 0))

/*
** dupq_22_b16:
** (
**	pfalse	(p[0-7])\.b
**	ptrue	(p[0-7])\.d, all
**	trn1	p0\.h, \1\.h, \2\.h
** |
**	ptrue	(p[0-7])\.d, all
**	pfalse	(p[0-7])\.b
**	trn1	p0\.h, \4\.h, \3\.h
** )
**	ret
*/
TEST_UNIFORM_P (dupq_22_b16,
		p0 = svdupq_n_b16 (0, 1, 0, 0, 0, 1, 0, 0),
		p0 = svdupq_b16 (0, 1, 0, 0, 0, 1, 0, 0))

/*
** dupq_33_b16:
**	ptrue	(p[0-7])\.d, all
**	trn1	p0\.h, \1\.h, \1\.h
**	ret
*/
TEST_UNIFORM_P (dupq_33_b16,
		p0 = svdupq_n_b16 (1, 1, 0, 0, 1, 1, 0, 0),
		p0 = svdupq_b16 (1, 1, 0, 0, 1, 1, 0, 0))

/*
** dupq_44_b16:
** (
**	ptrue	(p[0-7])\.d, all
**	ptrue	(p[0-7])\.s, all
**	not	p0\.b, \2/z, \1\.b
** |
**	ptrue	(p[0-7])\.s, all
**	ptrue	(p[0-7])\.d, all
**	not	p0\.b, \3/z, \4\.b
** )
**	ret
*/
TEST_UNIFORM_P (dupq_44_b16,
		p0 = svdupq_n_b16 (0, 0, 1, 0, 0, 0, 1, 0),
		p0 = svdupq_b16 (0, 0, 1, 0, 0, 0, 1, 0))

/*
** dupq_55_b16:
**	ptrue	p0\.s, all
**	ret
*/
TEST_UNIFORM_P (dupq_55_b16,
		p0 = svdupq_n_b16 (1, 0, 1, 0, 1, 0, 1, 0),
		p0 = svdupq_b16 (1, 0, 1, 0, 1, 0, 1, 0))

/*
** dupq_66_b16:
**	...
**	cmpne	p0\.b, p[0-7]/z, z[0-9]+\.b, #0
**	ret
*/
TEST_UNIFORM_P (dupq_66_b16,
		p0 = svdupq_n_b16 (0, 1, 1, 0, 0, 1, 1, 0),
		p0 = svdupq_b16 (0, 1, 1, 0, 0, 1, 1, 0))

/*
** dupq_77_b16:
** (
**	ptrue	(p[0-7])\.d, all
**	ptrue	(p[0-7])\.[hs], all
**	trn1	p0\.h, \2\.h, \1\.h
** |
**	ptrue	(p[0-7])\.[hs], all
**	ptrue	(p[0-7])\.s, all
**	trn1	p0\.h, \3\.h, \4\.h
** )
**	ret
*/
TEST_UNIFORM_P (dupq_77_b16,
		p0 = svdupq_n_b16 (1, 1, 1, 0, 1, 1, 1, 0),
		p0 = svdupq_b16 (1, 1, 1, 0, 1, 1, 1, 0))

/*
** dupq_88_b16:
** (
**	mov	(z[0-9]+)\.d, #71776119061217280
**	ptrue	(p[0-7])\.b, all
**	cmpne	p0\.b, \2/z, \1\.b, #0
** |
**	ptrue	(p[0-7])\.b, all
**	mov	(z[0-9]+)\.d, #71776119061217280
**	cmpne	p0\.b, \3/z, \4\.b, #0
** )
**	ret
*/
TEST_UNIFORM_P (dupq_88_b16,
		p0 = svdupq_n_b16 (0, 0, 0, 1, 0, 0, 0, 1),
		p0 = svdupq_b16 (0, 0, 0, 1, 0, 0, 0, 1))

/*
** dupq_99_b16:
**	...
**	cmpne	p0\.b, p[0-7]/z, z[0-9]+\.b, #0
**	ret
*/
TEST_UNIFORM_P (dupq_99_b16,
		p0 = svdupq_n_b16 (1, 0, 0, 1, 1, 0, 0, 1),
		p0 = svdupq_b16 (1, 0, 0, 1, 1, 0, 0, 1))

/*
** dupq_aa_b16:
** (
**	ptrue	(p[0-7])\.s, all
**	ptrue	(p[0-7])\.h, all
**	not	p0\.b, \2/z, \1\.b
** |
**	ptrue	(p[0-7])\.h, all
**	ptrue	(p[0-7])\.s, all
**	not	p0\.b, \3/z, \4\.b
** )
**	ret
*/
TEST_UNIFORM_P (dupq_aa_b16,
		p0 = svdupq_n_b16 (0, 1, 0, 1, 0, 1, 0, 1),
		p0 = svdupq_b16 (0, 1, 0, 1, 0, 1, 0, 1))

/*
** dupq_bb_b16:
** (
**	ptrue	(p[0-7])\.d, all
**	ptrue	(p[0-7])\.[hs], all
**	trn1	p0\.h, \1\.h, \2\.h
** |
**	ptrue	(p[0-7])\.[hs], all
**	ptrue	(p[0-7])\.d, all
**	trn1	p0\.h, \4\.h, \3\.h
** )
**	ret
*/
TEST_UNIFORM_P (dupq_bb_b16,
		p0 = svdupq_n_b16 (1, 1, 0, 1, 1, 1, 0, 1),
		p0 = svdupq_b16 (1, 1, 0, 1, 1, 1, 0, 1))

/*
** dupq_cc_b16:
** (
**	pfalse	(p[0-7])\.b
**	ptrue	(p[0-7])\.h, all
**	trn1	p0\.s, \1\.s, \2\.s
** |
**	ptrue	(p[0-7])\.h, all
**	pfalse	(p[0-7])\.b
**	trn1	p0\.s, \4\.s, \3\.s
** )
**	ret
*/
TEST_UNIFORM_P (dupq_cc_b16,
		p0 = svdupq_n_b16 (0, 0, 1, 1, 0, 0, 1, 1),
		p0 = svdupq_b16 (0, 0, 1, 1, 0, 0, 1, 1))

/*
** dupq_dd_b16:
** (
**	ptrue	(p[0-7])\.[sd], all
**	ptrue	(p[0-7])\.h, all
**	trn1	p0\.s, \1\.s, \2\.s
** |
**	ptrue	(p[0-7])\.h, all
**	ptrue	(p[0-7])\.[sd], all
**	trn1	p0\.s, \4\.s, \3\.s
** )
**	ret
*/
TEST_UNIFORM_P (dupq_dd_b16,
		p0 = svdupq_n_b16 (1, 0, 1, 1, 1, 0, 1, 1),
		p0 = svdupq_b16 (1, 0, 1, 1, 1, 0, 1, 1))

/*
** dupq_ee_b16:
** (
**	ptrue	(p[0-7])\.d, all
**	ptrue	(p[0-7])\.h, all
**	not	p0\.b, \2/z, \1\.b
** |
**	ptrue	(p[0-7])\.h, all
**	ptrue	(p[0-7])\.d, all
**	not	p0\.b, \3/z, \4\.b
** )
**	ret
*/
TEST_UNIFORM_P (dupq_ee_b16,
		p0 = svdupq_n_b16 (0, 1, 1, 1, 0, 1, 1, 1),
		p0 = svdupq_b16 (0, 1, 1, 1, 0, 1, 1, 1))

/*
** dupq_ff_b16:
**	ptrue	p0\.h, all
**	ret
*/
TEST_UNIFORM_P (dupq_ff_b16,
		p0 = svdupq_n_b16 (1, 1, 1, 1, 1, 1, 1, 1),
		p0 = svdupq_b16 (1, 1, 1, 1, 1, 1, 1, 1))

/*
** dupq_01_b16:
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
TEST_UNIFORM_P (dupq_01_b16,
		p0 = svdupq_n_b16 (1, 0, 0, 0, 0, 0, 0, 0),
		p0 = svdupq_b16 (1, 0, 0, 0, 0, 0, 0, 0))

/*
** dupq_03_b16:
**	...
**	cmpne	p0\.b, p[0-7]/z, z[0-9]+\.b, #0
**	ret
*/
TEST_UNIFORM_P (dupq_03_b16,
		p0 = svdupq_n_b16 (1, 1, 0, 0, 0, 0, 0, 0),
		p0 = svdupq_b16 (1, 1, 0, 0, 0, 0, 0, 0))

/*
** dupq_0f_b16:
** (
**	ptrue	(p[0-7])\.h, all
**	pfalse	(p[0-7])\.b
**	trn1	p0\.d, \1\.d, \2\.d
** |
**	pfalse	(p[0-7])\.b
**	ptrue	(p[0-7])\.h, all
**	trn1	p0\.d, \4\.d, \3\.d
** )
**	ret
*/
TEST_UNIFORM_P (dupq_0f_b16,
		p0 = svdupq_n_b16 (1, 1, 1, 1, 0, 0, 0, 0),
		p0 = svdupq_b16 (1, 1, 1, 1, 0, 0, 0, 0))

/*
** dupq_3f_b16:
**	...
**	cmpne	p0\.b, p[0-7]/z, z[0-9]+\.b, #0
**	ret
*/
TEST_UNIFORM_P (dupq_3f_b16,
		p0 = svdupq_n_b16 (1, 1, 1, 1, 1, 1, 0, 0),
		p0 = svdupq_b16 (1, 1, 1, 1, 1, 1, 0, 0))
