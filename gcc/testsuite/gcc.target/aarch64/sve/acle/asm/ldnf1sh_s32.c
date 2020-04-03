/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldnf1sh_s32_base:
**	ldnf1sh	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1sh_s32_base, svint32_t, int16_t,
	   z0 = svldnf1sh_s32 (p0, x0),
	   z0 = svldnf1sh_s32 (p0, x0))

/*
** ldnf1sh_s32_index:
**	add	(x[0-9]+), x0, x1, lsl 1
**	ldnf1sh	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ldnf1sh_s32_index, svint32_t, int16_t,
	   z0 = svldnf1sh_s32 (p0, x0 + x1),
	   z0 = svldnf1sh_s32 (p0, x0 + x1))

/*
** ldnf1sh_s32_1:
**	ldnf1sh	z0\.s, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1sh_s32_1, svint32_t, int16_t,
	   z0 = svldnf1sh_s32 (p0, x0 + svcntw ()),
	   z0 = svldnf1sh_s32 (p0, x0 + svcntw ()))

/*
** ldnf1sh_s32_7:
**	ldnf1sh	z0\.s, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1sh_s32_7, svint32_t, int16_t,
	   z0 = svldnf1sh_s32 (p0, x0 + svcntw () * 7),
	   z0 = svldnf1sh_s32 (p0, x0 + svcntw () * 7))

/*
** ldnf1sh_s32_8:
**	incb	x0, all, mul #4
**	ldnf1sh	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1sh_s32_8, svint32_t, int16_t,
	   z0 = svldnf1sh_s32 (p0, x0 + svcntw () * 8),
	   z0 = svldnf1sh_s32 (p0, x0 + svcntw () * 8))

/*
** ldnf1sh_s32_m1:
**	ldnf1sh	z0\.s, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1sh_s32_m1, svint32_t, int16_t,
	   z0 = svldnf1sh_s32 (p0, x0 - svcntw ()),
	   z0 = svldnf1sh_s32 (p0, x0 - svcntw ()))

/*
** ldnf1sh_s32_m8:
**	ldnf1sh	z0\.s, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1sh_s32_m8, svint32_t, int16_t,
	   z0 = svldnf1sh_s32 (p0, x0 - svcntw () * 8),
	   z0 = svldnf1sh_s32 (p0, x0 - svcntw () * 8))

/*
** ldnf1sh_s32_m9:
**	dech	x0, all, mul #9
**	ldnf1sh	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1sh_s32_m9, svint32_t, int16_t,
	   z0 = svldnf1sh_s32 (p0, x0 - svcntw () * 9),
	   z0 = svldnf1sh_s32 (p0, x0 - svcntw () * 9))

/*
** ldnf1sh_vnum_s32_0:
**	ldnf1sh	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1sh_vnum_s32_0, svint32_t, int16_t,
	   z0 = svldnf1sh_vnum_s32 (p0, x0, 0),
	   z0 = svldnf1sh_vnum_s32 (p0, x0, 0))

/*
** ldnf1sh_vnum_s32_1:
**	ldnf1sh	z0\.s, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1sh_vnum_s32_1, svint32_t, int16_t,
	   z0 = svldnf1sh_vnum_s32 (p0, x0, 1),
	   z0 = svldnf1sh_vnum_s32 (p0, x0, 1))

/*
** ldnf1sh_vnum_s32_7:
**	ldnf1sh	z0\.s, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1sh_vnum_s32_7, svint32_t, int16_t,
	   z0 = svldnf1sh_vnum_s32 (p0, x0, 7),
	   z0 = svldnf1sh_vnum_s32 (p0, x0, 7))

/*
** ldnf1sh_vnum_s32_8:
**	incb	x0, all, mul #4
**	ldnf1sh	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1sh_vnum_s32_8, svint32_t, int16_t,
	   z0 = svldnf1sh_vnum_s32 (p0, x0, 8),
	   z0 = svldnf1sh_vnum_s32 (p0, x0, 8))

/*
** ldnf1sh_vnum_s32_m1:
**	ldnf1sh	z0\.s, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1sh_vnum_s32_m1, svint32_t, int16_t,
	   z0 = svldnf1sh_vnum_s32 (p0, x0, -1),
	   z0 = svldnf1sh_vnum_s32 (p0, x0, -1))

/*
** ldnf1sh_vnum_s32_m8:
**	ldnf1sh	z0\.s, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1sh_vnum_s32_m8, svint32_t, int16_t,
	   z0 = svldnf1sh_vnum_s32 (p0, x0, -8),
	   z0 = svldnf1sh_vnum_s32 (p0, x0, -8))

/*
** ldnf1sh_vnum_s32_m9:
**	dech	x0, all, mul #9
**	ldnf1sh	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1sh_vnum_s32_m9, svint32_t, int16_t,
	   z0 = svldnf1sh_vnum_s32 (p0, x0, -9),
	   z0 = svldnf1sh_vnum_s32 (p0, x0, -9))

/*
** ldnf1sh_vnum_s32_x1:
**	cnth	(x[0-9]+)
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldnf1sh	z0\.s, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ldnf1sh_vnum_s32_x1, svint32_t, int16_t,
	   z0 = svldnf1sh_vnum_s32 (p0, x0, x1),
	   z0 = svldnf1sh_vnum_s32 (p0, x0, x1))
