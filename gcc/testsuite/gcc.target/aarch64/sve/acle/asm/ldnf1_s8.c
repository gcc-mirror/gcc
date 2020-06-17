/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldnf1_s8_base:
**	ldnf1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_s8_base, svint8_t, int8_t,
	   z0 = svldnf1_s8 (p0, x0),
	   z0 = svldnf1 (p0, x0))

/*
** ldnf1_s8_index:
**	add	(x[0-9]+), x0, x1
**	ldnf1b	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ldnf1_s8_index, svint8_t, int8_t,
	   z0 = svldnf1_s8 (p0, x0 + x1),
	   z0 = svldnf1 (p0, x0 + x1))

/*
** ldnf1_s8_1:
**	ldnf1b	z0\.b, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_s8_1, svint8_t, int8_t,
	   z0 = svldnf1_s8 (p0, x0 + svcntb ()),
	   z0 = svldnf1 (p0, x0 + svcntb ()))

/*
** ldnf1_s8_7:
**	ldnf1b	z0\.b, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_s8_7, svint8_t, int8_t,
	   z0 = svldnf1_s8 (p0, x0 + svcntb () * 7),
	   z0 = svldnf1 (p0, x0 + svcntb () * 7))

/*
** ldnf1_s8_8:
**	incb	x0, all, mul #8
**	ldnf1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_s8_8, svint8_t, int8_t,
	   z0 = svldnf1_s8 (p0, x0 + svcntb () * 8),
	   z0 = svldnf1 (p0, x0 + svcntb () * 8))

/*
** ldnf1_s8_m1:
**	ldnf1b	z0\.b, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_s8_m1, svint8_t, int8_t,
	   z0 = svldnf1_s8 (p0, x0 - svcntb ()),
	   z0 = svldnf1 (p0, x0 - svcntb ()))

/*
** ldnf1_s8_m8:
**	ldnf1b	z0\.b, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_s8_m8, svint8_t, int8_t,
	   z0 = svldnf1_s8 (p0, x0 - svcntb () * 8),
	   z0 = svldnf1 (p0, x0 - svcntb () * 8))

/*
** ldnf1_s8_m9:
**	decb	x0, all, mul #9
**	ldnf1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_s8_m9, svint8_t, int8_t,
	   z0 = svldnf1_s8 (p0, x0 - svcntb () * 9),
	   z0 = svldnf1 (p0, x0 - svcntb () * 9))

/*
** ldnf1_vnum_s8_0:
**	ldnf1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_s8_0, svint8_t, int8_t,
	   z0 = svldnf1_vnum_s8 (p0, x0, 0),
	   z0 = svldnf1_vnum (p0, x0, 0))

/*
** ldnf1_vnum_s8_1:
**	ldnf1b	z0\.b, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_s8_1, svint8_t, int8_t,
	   z0 = svldnf1_vnum_s8 (p0, x0, 1),
	   z0 = svldnf1_vnum (p0, x0, 1))

/*
** ldnf1_vnum_s8_7:
**	ldnf1b	z0\.b, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_s8_7, svint8_t, int8_t,
	   z0 = svldnf1_vnum_s8 (p0, x0, 7),
	   z0 = svldnf1_vnum (p0, x0, 7))

/*
** ldnf1_vnum_s8_8:
**	incb	x0, all, mul #8
**	ldnf1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_s8_8, svint8_t, int8_t,
	   z0 = svldnf1_vnum_s8 (p0, x0, 8),
	   z0 = svldnf1_vnum (p0, x0, 8))

/*
** ldnf1_vnum_s8_m1:
**	ldnf1b	z0\.b, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_s8_m1, svint8_t, int8_t,
	   z0 = svldnf1_vnum_s8 (p0, x0, -1),
	   z0 = svldnf1_vnum (p0, x0, -1))

/*
** ldnf1_vnum_s8_m8:
**	ldnf1b	z0\.b, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_s8_m8, svint8_t, int8_t,
	   z0 = svldnf1_vnum_s8 (p0, x0, -8),
	   z0 = svldnf1_vnum (p0, x0, -8))

/*
** ldnf1_vnum_s8_m9:
**	decb	x0, all, mul #9
**	ldnf1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_s8_m9, svint8_t, int8_t,
	   z0 = svldnf1_vnum_s8 (p0, x0, -9),
	   z0 = svldnf1_vnum (p0, x0, -9))

/*
** ldnf1_vnum_s8_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldnf1b	z0\.b, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_s8_x1, svint8_t, int8_t,
	   z0 = svldnf1_vnum_s8 (p0, x0, x1),
	   z0 = svldnf1_vnum (p0, x0, x1))
