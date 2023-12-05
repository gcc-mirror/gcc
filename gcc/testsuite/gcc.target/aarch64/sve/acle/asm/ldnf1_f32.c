/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldnf1_f32_base:
**	ldnf1w	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_f32_base, svfloat32_t, float32_t,
	   z0 = svldnf1_f32 (p0, x0),
	   z0 = svldnf1 (p0, x0))

/*
** ldnf1_f32_index:
**	add	(x[0-9]+), x0, x1, lsl 2
**	ldnf1w	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ldnf1_f32_index, svfloat32_t, float32_t,
	   z0 = svldnf1_f32 (p0, x0 + x1),
	   z0 = svldnf1 (p0, x0 + x1))

/*
** ldnf1_f32_1:
**	ldnf1w	z0\.s, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_f32_1, svfloat32_t, float32_t,
	   z0 = svldnf1_f32 (p0, x0 + svcntw ()),
	   z0 = svldnf1 (p0, x0 + svcntw ()))

/*
** ldnf1_f32_7:
**	ldnf1w	z0\.s, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_f32_7, svfloat32_t, float32_t,
	   z0 = svldnf1_f32 (p0, x0 + svcntw () * 7),
	   z0 = svldnf1 (p0, x0 + svcntw () * 7))

/*
** ldnf1_f32_8:
**	incb	x0, all, mul #8
**	ldnf1w	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_f32_8, svfloat32_t, float32_t,
	   z0 = svldnf1_f32 (p0, x0 + svcntw () * 8),
	   z0 = svldnf1 (p0, x0 + svcntw () * 8))

/*
** ldnf1_f32_m1:
**	ldnf1w	z0\.s, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_f32_m1, svfloat32_t, float32_t,
	   z0 = svldnf1_f32 (p0, x0 - svcntw ()),
	   z0 = svldnf1 (p0, x0 - svcntw ()))

/*
** ldnf1_f32_m8:
**	ldnf1w	z0\.s, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_f32_m8, svfloat32_t, float32_t,
	   z0 = svldnf1_f32 (p0, x0 - svcntw () * 8),
	   z0 = svldnf1 (p0, x0 - svcntw () * 8))

/*
** ldnf1_f32_m9:
**	decb	x0, all, mul #9
**	ldnf1w	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_f32_m9, svfloat32_t, float32_t,
	   z0 = svldnf1_f32 (p0, x0 - svcntw () * 9),
	   z0 = svldnf1 (p0, x0 - svcntw () * 9))

/*
** ldnf1_vnum_f32_0:
**	ldnf1w	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_f32_0, svfloat32_t, float32_t,
	   z0 = svldnf1_vnum_f32 (p0, x0, 0),
	   z0 = svldnf1_vnum (p0, x0, 0))

/*
** ldnf1_vnum_f32_1:
**	ldnf1w	z0\.s, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_f32_1, svfloat32_t, float32_t,
	   z0 = svldnf1_vnum_f32 (p0, x0, 1),
	   z0 = svldnf1_vnum (p0, x0, 1))

/*
** ldnf1_vnum_f32_7:
**	ldnf1w	z0\.s, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_f32_7, svfloat32_t, float32_t,
	   z0 = svldnf1_vnum_f32 (p0, x0, 7),
	   z0 = svldnf1_vnum (p0, x0, 7))

/*
** ldnf1_vnum_f32_8:
**	incb	x0, all, mul #8
**	ldnf1w	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_f32_8, svfloat32_t, float32_t,
	   z0 = svldnf1_vnum_f32 (p0, x0, 8),
	   z0 = svldnf1_vnum (p0, x0, 8))

/*
** ldnf1_vnum_f32_m1:
**	ldnf1w	z0\.s, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_f32_m1, svfloat32_t, float32_t,
	   z0 = svldnf1_vnum_f32 (p0, x0, -1),
	   z0 = svldnf1_vnum (p0, x0, -1))

/*
** ldnf1_vnum_f32_m8:
**	ldnf1w	z0\.s, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_f32_m8, svfloat32_t, float32_t,
	   z0 = svldnf1_vnum_f32 (p0, x0, -8),
	   z0 = svldnf1_vnum (p0, x0, -8))

/*
** ldnf1_vnum_f32_m9:
**	decb	x0, all, mul #9
**	ldnf1w	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_f32_m9, svfloat32_t, float32_t,
	   z0 = svldnf1_vnum_f32 (p0, x0, -9),
	   z0 = svldnf1_vnum (p0, x0, -9))

/*
** ldnf1_vnum_f32_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldnf1w	z0\.s, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_f32_x1, svfloat32_t, float32_t,
	   z0 = svldnf1_vnum_f32 (p0, x0, x1),
	   z0 = svldnf1_vnum (p0, x0, x1))
