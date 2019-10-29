/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ldnf1_u16_base:
**	ldnf1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_u16_base, svuint16_t, uint16_t,
	   z0 = svldnf1_u16 (p0, x0),
	   z0 = svldnf1 (p0, x0))

/*
** ldnf1_u16_index:
**	add	(x[0-9]+), x0, x1, lsl 1
**	ldnf1h	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ldnf1_u16_index, svuint16_t, uint16_t,
	   z0 = svldnf1_u16 (p0, x0 + x1),
	   z0 = svldnf1 (p0, x0 + x1))

/*
** ldnf1_u16_1:
**	ldnf1h	z0\.h, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_u16_1, svuint16_t, uint16_t,
	   z0 = svldnf1_u16 (p0, x0 + svcnth ()),
	   z0 = svldnf1 (p0, x0 + svcnth ()))

/*
** ldnf1_u16_7:
**	ldnf1h	z0\.h, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_u16_7, svuint16_t, uint16_t,
	   z0 = svldnf1_u16 (p0, x0 + svcnth () * 7),
	   z0 = svldnf1 (p0, x0 + svcnth () * 7))

/*
** ldnf1_u16_8:
**	incb	x0, all, mul #8
**	ldnf1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_u16_8, svuint16_t, uint16_t,
	   z0 = svldnf1_u16 (p0, x0 + svcnth () * 8),
	   z0 = svldnf1 (p0, x0 + svcnth () * 8))

/*
** ldnf1_u16_m1:
**	ldnf1h	z0\.h, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_u16_m1, svuint16_t, uint16_t,
	   z0 = svldnf1_u16 (p0, x0 - svcnth ()),
	   z0 = svldnf1 (p0, x0 - svcnth ()))

/*
** ldnf1_u16_m8:
**	ldnf1h	z0\.h, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_u16_m8, svuint16_t, uint16_t,
	   z0 = svldnf1_u16 (p0, x0 - svcnth () * 8),
	   z0 = svldnf1 (p0, x0 - svcnth () * 8))

/*
** ldnf1_u16_m9:
**	decb	x0, all, mul #9
**	ldnf1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_u16_m9, svuint16_t, uint16_t,
	   z0 = svldnf1_u16 (p0, x0 - svcnth () * 9),
	   z0 = svldnf1 (p0, x0 - svcnth () * 9))

/*
** ldnf1_vnum_u16_0:
**	ldnf1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_u16_0, svuint16_t, uint16_t,
	   z0 = svldnf1_vnum_u16 (p0, x0, 0),
	   z0 = svldnf1_vnum (p0, x0, 0))

/*
** ldnf1_vnum_u16_1:
**	ldnf1h	z0\.h, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_u16_1, svuint16_t, uint16_t,
	   z0 = svldnf1_vnum_u16 (p0, x0, 1),
	   z0 = svldnf1_vnum (p0, x0, 1))

/*
** ldnf1_vnum_u16_7:
**	ldnf1h	z0\.h, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_u16_7, svuint16_t, uint16_t,
	   z0 = svldnf1_vnum_u16 (p0, x0, 7),
	   z0 = svldnf1_vnum (p0, x0, 7))

/*
** ldnf1_vnum_u16_8:
**	incb	x0, all, mul #8
**	ldnf1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_u16_8, svuint16_t, uint16_t,
	   z0 = svldnf1_vnum_u16 (p0, x0, 8),
	   z0 = svldnf1_vnum (p0, x0, 8))

/*
** ldnf1_vnum_u16_m1:
**	ldnf1h	z0\.h, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_u16_m1, svuint16_t, uint16_t,
	   z0 = svldnf1_vnum_u16 (p0, x0, -1),
	   z0 = svldnf1_vnum (p0, x0, -1))

/*
** ldnf1_vnum_u16_m8:
**	ldnf1h	z0\.h, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_u16_m8, svuint16_t, uint16_t,
	   z0 = svldnf1_vnum_u16 (p0, x0, -8),
	   z0 = svldnf1_vnum (p0, x0, -8))

/*
** ldnf1_vnum_u16_m9:
**	decb	x0, all, mul #9
**	ldnf1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_u16_m9, svuint16_t, uint16_t,
	   z0 = svldnf1_vnum_u16 (p0, x0, -9),
	   z0 = svldnf1_vnum (p0, x0, -9))

/*
** ldnf1_vnum_u16_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldnf1h	z0\.h, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ldnf1_vnum_u16_x1, svuint16_t, uint16_t,
	   z0 = svldnf1_vnum_u16 (p0, x0, x1),
	   z0 = svldnf1_vnum (p0, x0, x1))
