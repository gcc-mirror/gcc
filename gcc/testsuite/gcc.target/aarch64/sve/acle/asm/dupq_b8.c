/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dupq_0000_b8:
**	pfalse	p0\.b
**	ret
*/
TEST_UNIFORM_P (dupq_0000_b8,
		p0 = svdupq_n_b8 (0, 0, 0, 0, 0, 0, 0, 0,
				  0, 0, 0, 0, 0, 0, 0, 0),
		p0 = svdupq_b8 (0, 0, 0, 0, 0, 0, 0, 0,
				0, 0, 0, 0, 0, 0, 0, 0))

/*
** dupq_1111_b8:
**	ptrue	p0\.s, all
**	ret
*/
TEST_UNIFORM_P (dupq_1111_b8,
		p0 = svdupq_n_b8 (1, 0, 0, 0, 1, 0, 0, 0,
				  1, 0, 0, 0, 1, 0, 0, 0),
		p0 = svdupq_b8 (1, 0, 0, 0, 1, 0, 0, 0,
				1, 0, 0, 0, 1, 0, 0, 0))

/*
** dupq_2222_b8:
** (
**	pfalse	(p[0-9]+)\.b
**	ptrue	(p[0-9]+)\.s, all
**	trn1	p0\.b, \1\.b, \2\.b
** |
**	ptrue	(p[0-9]+)\.s, all
**	pfalse	(p[0-9]+)\.b
**	trn1	p0\.b, \4\.b, \3\.b
** )
**	ret
*/
TEST_UNIFORM_P (dupq_2222_b8,
		p0 = svdupq_n_b8 (0, 1, 0, 0, 0, 1, 0, 0,
				  0, 1, 0, 0, 0, 1, 0, 0),
		p0 = svdupq_b8 (0, 1, 0, 0, 0, 1, 0, 0,
				0, 1, 0, 0, 0, 1, 0, 0))

/*
** dupq_3333_b8:
**	ptrue	(p[0-9]+)\.s, all
**	trn1	p0\.b, \1\.b, \1\.b
**	ret
*/
TEST_UNIFORM_P (dupq_3333_b8,
		p0 = svdupq_n_b8 (1, 1, 0, 0, 1, 1, 0, 0,
				  1, 1, 0, 0, 1, 1, 0, 0),
		p0 = svdupq_b8 (1, 1, 0, 0, 1, 1, 0, 0,
				1, 1, 0, 0, 1, 1, 0, 0))

/*
** dupq_4444_b8:
** (
**	ptrue	(p[0-9]+)\.s, all
**	ptrue	(p[0-9]+)\.h, all
**	not	p0\.b, \2/z, \1\.b
** |
**	ptrue	(p[0-9]+)\.h, all
**	ptrue	(p[0-9]+)\.s, all
**	not	p0\.b, \3/z, \4\.b
** )
**	ret
*/
TEST_UNIFORM_P (dupq_4444_b8,
		p0 = svdupq_n_b8 (0, 0, 1, 0, 0, 0, 1, 0,
				  0, 0, 1, 0, 0, 0, 1, 0),
		p0 = svdupq_b8 (0, 0, 1, 0, 0, 0, 1, 0,
				0, 0, 1, 0, 0, 0, 1, 0))

/*
** dupq_5555_b8:
**	ptrue	p0\.h, all
**	ret
*/
TEST_UNIFORM_P (dupq_5555_b8,
		p0 = svdupq_n_b8 (1, 0, 1, 0, 1, 0, 1, 0,
				  1, 0, 1, 0, 1, 0, 1, 0),
		p0 = svdupq_b8 (1, 0, 1, 0, 1, 0, 1, 0,
				1, 0, 1, 0, 1, 0, 1, 0))

/*
** dupq_6666_b8:
** (
**	mov	(z[0-9]+)\.s, #16776960
**	ptrue	(p[0-9]+)\.b, all
**	cmpne	p0\.b, \2/z, \1\.b, #0
** |
**	ptrue	(p[0-9]+)\.b, all
**	mov	(z[0-9]+)\.s, #16776960
**	cmpne	p0\.b, \3/z, \4\.b, #0
** )
**	ret
*/
TEST_UNIFORM_P (dupq_6666_b8,
		p0 = svdupq_n_b8 (0, 1, 1, 0, 0, 1, 1, 0,
				  0, 1, 1, 0, 0, 1, 1, 0),
		p0 = svdupq_b8 (0, 1, 1, 0, 0, 1, 1, 0,
				0, 1, 1, 0, 0, 1, 1, 0))

/*
** dupq_7777_b8:
** (
**	ptrue	(p[0-9]+)\.s, all
**	ptrue	(p[0-9]+)\.[bh], all
**	trn1	p0\.b, \2\.b, \1\.b
** |
**	ptrue	(p[0-9]+)\.[bh], all
**	ptrue	(p[0-9]+)\.s, all
**	trn1	p0\.b, \3\.b, \4\.b
** )
**	ret
*/
TEST_UNIFORM_P (dupq_7777_b8,
		p0 = svdupq_n_b8 (1, 1, 1, 0, 1, 1, 1, 0,
				  1, 1, 1, 0, 1, 1, 1, 0),
		p0 = svdupq_b8 (1, 1, 1, 0, 1, 1, 1, 0,
				1, 1, 1, 0, 1, 1, 1, 0))

/*
** dupq_8888_b8:
** (
**	mov	(z[0-9]+)\.s, #-16777216
**	ptrue	(p[0-9]+)\.b, all
**	cmpne	p0\.b, \2/z, \1\.b, #0
** |
**	ptrue	(p[0-9]+)\.b, all
**	mov	(z[0-9]+)\.s, #-16777216
**	cmpne	p0\.b, \3/z, \4\.b, #0
** )
**	ret
*/
TEST_UNIFORM_P (dupq_8888_b8,
		p0 = svdupq_n_b8 (0, 0, 0, 1, 0, 0, 0, 1,
				  0, 0, 0, 1, 0, 0, 0, 1),
		p0 = svdupq_b8 (0, 0, 0, 1, 0, 0, 0, 1,
				0, 0, 0, 1, 0, 0, 0, 1))

/*
** dupq_9999_b8:
** (
**	mov	(z[0-9]+)\.s, #-16776961
**	ptrue	(p[0-9]+)\.b, all
**	cmpne	p0\.b, \2/z, \1\.b, #0
** |
**	ptrue	(p[0-9]+)\.b, all
**	mov	(z[0-9]+)\.s, #-16776961
**	cmpne	p0\.b, \3/z, \4\.b, #0
** )
**	ret
*/
TEST_UNIFORM_P (dupq_9999_b8,
		p0 = svdupq_n_b8 (1, 0, 0, 1, 1, 0, 0, 1,
				  1, 0, 0, 1, 1, 0, 0, 1),
		p0 = svdupq_b8 (1, 0, 0, 1, 1, 0, 0, 1,
				1, 0, 0, 1, 1, 0, 0, 1))

/*
** dupq_aaaa_b8:
** (
**	ptrue	(p[0-9]+)\.h, all
**	ptrue	(p[0-9]+)\.b, all
**	not	p0\.b, \2/z, \1\.b
** |
**	ptrue	(p[0-9]+)\.b, all
**	ptrue	(p[0-9]+)\.h, all
**	not	p0\.b, \3/z, \4\.b
** )
**	ret
*/
TEST_UNIFORM_P (dupq_aaaa_b8,
		p0 = svdupq_n_b8 (0, 1, 0, 1, 0, 1, 0, 1,
				  0, 1, 0, 1, 0, 1, 0, 1),
		p0 = svdupq_b8 (0, 1, 0, 1, 0, 1, 0, 1,
				0, 1, 0, 1, 0, 1, 0, 1))

/*
** dupq_bbbb_b8:
** (
**	ptrue	(p[0-9]+)\.s, all
**	ptrue	(p[0-9]+)\.[bh], all
**	trn1	p0\.b, \1\.b, \2\.b
** |
**	ptrue	(p[0-9]+)\.[bh], all
**	ptrue	(p[0-9]+)\.s, all
**	trn1	p0\.b, \4\.b, \3\.b
** )
**	ret
*/
TEST_UNIFORM_P (dupq_bbbb_b8,
		p0 = svdupq_n_b8 (1, 1, 0, 1, 1, 1, 0, 1,
				  1, 1, 0, 1, 1, 1, 0, 1),
		p0 = svdupq_b8 (1, 1, 0, 1, 1, 1, 0, 1,
				1, 1, 0, 1, 1, 1, 0, 1))

/*
** dupq_cccc_b8:
** (
**	pfalse	(p[0-9]+)\.b
**	ptrue	(p[0-9]+)\.b, all
**	trn1	p0\.h, \1\.h, \2\.h
** |
**	ptrue	(p[0-9]+)\.b, all
**	pfalse	(p[0-9]+)\.b
**	trn1	p0\.h, \4\.h, \3\.h
** )
**	ret
*/
TEST_UNIFORM_P (dupq_cccc_b8,
		p0 = svdupq_n_b8 (0, 0, 1, 1, 0, 0, 1, 1,
				  0, 0, 1, 1, 0, 0, 1, 1),
		p0 = svdupq_b8 (0, 0, 1, 1, 0, 0, 1, 1,
				0, 0, 1, 1, 0, 0, 1, 1))

/*
** dupq_dddd_b8:
** (
**	ptrue	(p[0-9]+)\.[hs], all
**	ptrue	(p[0-9]+)\.b, all
**	trn1	p0\.h, \1\.h, \2\.h
** |
**	ptrue	(p[0-9]+)\.b, all
**	ptrue	(p[0-9]+)\.[hs], all
**	trn1	p0\.h, \4\.h, \3\.h
** )
**	ret
*/
TEST_UNIFORM_P (dupq_dddd_b8,
		p0 = svdupq_n_b8 (1, 0, 1, 1, 1, 0, 1, 1,
				  1, 0, 1, 1, 1, 0, 1, 1),
		p0 = svdupq_b8 (1, 0, 1, 1, 1, 0, 1, 1,
				1, 0, 1, 1, 1, 0, 1, 1))

/*
** dupq_eeee_b8:
** (
**	ptrue	(p[0-9]+)\.s, all
**	ptrue	(p[0-9]+)\.b, all
**	not	p0\.b, \2/z, \1\.b
** |
**	ptrue	(p[0-9]+)\.b, all
**	ptrue	(p[0-9]+)\.s, all
**	not	p0\.b, \3/z, \4\.b
** )
**	ret
*/
TEST_UNIFORM_P (dupq_eeee_b8,
		p0 = svdupq_n_b8 (0, 1, 1, 1, 0, 1, 1, 1,
				  0, 1, 1, 1, 0, 1, 1, 1),
		p0 = svdupq_b8 (0, 1, 1, 1, 0, 1, 1, 1,
				0, 1, 1, 1, 0, 1, 1, 1))

/*
** dupq_ffff_b8:
**	ptrue	p0\.b, all
**	ret
*/
TEST_UNIFORM_P (dupq_ffff_b8,
		p0 = svdupq_n_b8 (1, 1, 1, 1, 1, 1, 1, 1,
				  1, 1, 1, 1, 1, 1, 1, 1),
		p0 = svdupq_b8 (1, 1, 1, 1, 1, 1, 1, 1,
				1, 1, 1, 1, 1, 1, 1, 1))

/*
** dupq_5f5f_b8:
** (
**	ptrue	(p[0-9]+)\.h, all
**	ptrue	(p[0-9]+)\.b, all
**	trn1	p0\.s, \2\.s, \1\.s
** |
**	ptrue	(p[0-9]+)\.b, all
**	ptrue	(p[0-9]+)\.h, all
**	trn1	p0\.s, \3\.s, \4\.s
** )
**	ret
*/
TEST_UNIFORM_P (dupq_5f5f_b8,
		p0 = svdupq_n_b8 (1, 1, 1, 1, 1, 0, 1, 0,
				  1, 1, 1, 1, 1, 0, 1, 0),
		p0 = svdupq_b8 (1, 1, 1, 1, 1, 0, 1, 0,
				1, 1, 1, 1, 1, 0, 1, 0))

/*
** dupq_1f1f_b8:
** (
**	ptrue	(p[0-9]+)\.[sd], all
**	ptrue	(p[0-9]+)\.b, all
**	trn1	p0\.s, \2\.s, \1\.s
** |
**	ptrue	(p[0-9]+)\.b, all
**	ptrue	(p[0-9]+)\.[sd], all
**	trn1	p0\.s, \3\.s, \4\.s
** )
**	ret
*/
TEST_UNIFORM_P (dupq_1f1f_b8,
		p0 = svdupq_n_b8 (1, 1, 1, 1, 1, 0, 0, 0,
				  1, 1, 1, 1, 1, 0, 0, 0),
		p0 = svdupq_b8 (1, 1, 1, 1, 1, 0, 0, 0,
				1, 1, 1, 1, 1, 0, 0, 0))

/*
** dupq_1515_b8:
** (
**	ptrue	(p[0-9]+)\.d, all
**	ptrue	(p[0-9]+)\.[hs], all
**	trn1	p0\.h, \2\.h, \1\.h
** |
**	ptrue	(p[0-9]+)\.[hs], all
**	ptrue	(p[0-9]+)\.d, all
**	trn1	p0\.h, \3\.h, \4\.h
** )
**	ret
*/
TEST_UNIFORM_P (dupq_1515_b8,
		p0 = svdupq_n_b8 (1, 0, 1, 0, 1, 0, 0, 0,
				  1, 0, 1, 0, 1, 0, 0, 0),
		p0 = svdupq_b8 (1, 0, 1, 0, 1, 0, 0, 0,
				1, 0, 1, 0, 1, 0, 0, 0))

/*
** dupq_0505_b8:
**	ptrue	(p[0-9]+)\.d, all
**	trn1	p0\.h, \1\.h, \1\.h
**	ret
*/
TEST_UNIFORM_P (dupq_0505_b8,
		p0 = svdupq_n_b8 (1, 0, 1, 0, 0, 0, 0, 0,
				  1, 0, 1, 0, 0, 0, 0, 0),
		p0 = svdupq_b8 (1, 0, 1, 0, 0, 0, 0, 0,
				1, 0, 1, 0, 0, 0, 0, 0))

/*
** dupq_00ff_b8:
** (
**	pfalse	(p[0-9]+)\.b
**	ptrue	(p[0-9]+)\.b, all
**	trn1	p0\.d, \2\.d, \1\.d
** |
**	ptrue	(p[0-9]+)\.b, all
**	pfalse	(p[0-9]+)\.b
**	trn1	p0\.d, \3\.d, \4\.d
** )
**	ret
*/
TEST_UNIFORM_P (dupq_00ff_b8,
		p0 = svdupq_n_b8 (1, 1, 1, 1, 1, 1, 1, 1,
				  0, 0, 0, 0, 0, 0, 0, 0),
		p0 = svdupq_b8 (1, 1, 1, 1, 1, 1, 1, 1,
				0, 0, 0, 0, 0, 0, 0, 0))

/*
** dupq_0055_b8:
** (
**	pfalse	(p[0-9]+)\.b
**	ptrue	(p[0-9]+)\.h, all
**	trn1	p0\.d, \2\.d, \1\.d
** |
**	ptrue	(p[0-9]+)\.h, all
**	pfalse	(p[0-9]+)\.b
**	trn1	p0\.d, \3\.d, \4\.d
** )
**	ret
*/
TEST_UNIFORM_P (dupq_0055_b8,
		p0 = svdupq_n_b8 (1, 0, 1, 0, 1, 0, 1, 0,
				  0, 0, 0, 0, 0, 0, 0, 0),
		p0 = svdupq_b8 (1, 0, 1, 0, 1, 0, 1, 0,
				0, 0, 0, 0, 0, 0, 0, 0))

/*
** dupq_0011_b8:
** (
**	pfalse	(p[0-9]+)\.b
**	ptrue	(p[0-9]+)\.s, all
**	trn1	p0\.d, \2\.d, \1\.d
** |
**	ptrue	(p[0-9]+)\.s, all
**	pfalse	(p[0-9]+)\.b
**	trn1	p0\.d, \3\.d, \4\.d
** )
**	ret
*/
TEST_UNIFORM_P (dupq_0011_b8,
		p0 = svdupq_n_b8 (1, 0, 0, 0, 1, 0, 0, 0,
				  0, 0, 0, 0, 0, 0, 0, 0),
		p0 = svdupq_b8 (1, 0, 0, 0, 1, 0, 0, 0,
				0, 0, 0, 0, 0, 0, 0, 0))

/*
** dupq_0111_b8:
** (
**	ptrue	(p[0-9]+)\.d, all
**	ptrue	(p[0-9]+)\.s, all
**	trn1	p0\.d, \2\.d, \1\.d
** |
**	ptrue	(p[0-9]+)\.s, all
**	ptrue	(p[0-9]+)\.d, all
**	trn1	p0\.d, \3\.d, \4\.d
** )
**	ret
*/
TEST_UNIFORM_P (dupq_0111_b8,
		p0 = svdupq_n_b8 (1, 0, 0, 0, 1, 0, 0, 0,
				  1, 0, 0, 0, 0, 0, 0, 0),
		p0 = svdupq_b8 (1, 0, 0, 0, 1, 0, 0, 0,
				1, 0, 0, 0, 0, 0, 0, 0))
