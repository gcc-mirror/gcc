/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld3_u16_base:
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_u16_base, svuint16x3_t, uint16_t,
	   z0 = svld3_u16 (p0, x0),
	   z0 = svld3 (p0, x0))

/*
** ld3_u16_index:
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0, x1, lsl 1\]
**	ret
*/
TEST_LOAD (ld3_u16_index, svuint16x3_t, uint16_t,
	   z0 = svld3_u16 (p0, x0 + x1),
	   z0 = svld3 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_u16_1:
**	incb	x0
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_u16_1, svuint16x3_t, uint16_t,
	   z0 = svld3_u16 (p0, x0 + svcnth ()),
	   z0 = svld3 (p0, x0 + svcnth ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_u16_2:
**	incb	x0, all, mul #2
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_u16_2, svuint16x3_t, uint16_t,
	   z0 = svld3_u16 (p0, x0 + svcnth () * 2),
	   z0 = svld3 (p0, x0 + svcnth () * 2))

/*
** ld3_u16_3:
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0, #3, mul vl\]
**	ret
*/
TEST_LOAD (ld3_u16_3, svuint16x3_t, uint16_t,
	   z0 = svld3_u16 (p0, x0 + svcnth () * 3),
	   z0 = svld3 (p0, x0 + svcnth () * 3))

/*
** ld3_u16_21:
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0, #21, mul vl\]
**	ret
*/
TEST_LOAD (ld3_u16_21, svuint16x3_t, uint16_t,
	   z0 = svld3_u16 (p0, x0 + svcnth () * 21),
	   z0 = svld3 (p0, x0 + svcnth () * 21))

/*
** ld3_u16_24:
**	addvl	(x[0-9]+), x0, #24
**	ld3h	{z0\.h - z2\.h}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld3_u16_24, svuint16x3_t, uint16_t,
	   z0 = svld3_u16 (p0, x0 + svcnth () * 24),
	   z0 = svld3 (p0, x0 + svcnth () * 24))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_u16_m1:
**	decb	x0
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_u16_m1, svuint16x3_t, uint16_t,
	   z0 = svld3_u16 (p0, x0 - svcnth ()),
	   z0 = svld3 (p0, x0 - svcnth ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_u16_m2:
**	decb	x0, all, mul #2
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_u16_m2, svuint16x3_t, uint16_t,
	   z0 = svld3_u16 (p0, x0 - svcnth () * 2),
	   z0 = svld3 (p0, x0 - svcnth () * 2))

/*
** ld3_u16_m3:
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0, #-3, mul vl\]
**	ret
*/
TEST_LOAD (ld3_u16_m3, svuint16x3_t, uint16_t,
	   z0 = svld3_u16 (p0, x0 - svcnth () * 3),
	   z0 = svld3 (p0, x0 - svcnth () * 3))

/*
** ld3_u16_m24:
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0, #-24, mul vl\]
**	ret
*/
TEST_LOAD (ld3_u16_m24, svuint16x3_t, uint16_t,
	   z0 = svld3_u16 (p0, x0 - svcnth () * 24),
	   z0 = svld3 (p0, x0 - svcnth () * 24))

/*
** ld3_u16_m27:
**	addvl	(x[0-9]+), x0, #-27
**	ld3h	{z0\.h - z2\.h}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld3_u16_m27, svuint16x3_t, uint16_t,
	   z0 = svld3_u16 (p0, x0 - svcnth () * 27),
	   z0 = svld3 (p0, x0 - svcnth () * 27))

/*
** ld3_vnum_u16_0:
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_vnum_u16_0, svuint16x3_t, uint16_t,
	   z0 = svld3_vnum_u16 (p0, x0, 0),
	   z0 = svld3_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_vnum_u16_1:
**	incb	x0
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_vnum_u16_1, svuint16x3_t, uint16_t,
	   z0 = svld3_vnum_u16 (p0, x0, 1),
	   z0 = svld3_vnum (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_vnum_u16_2:
**	incb	x0, all, mul #2
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_vnum_u16_2, svuint16x3_t, uint16_t,
	   z0 = svld3_vnum_u16 (p0, x0, 2),
	   z0 = svld3_vnum (p0, x0, 2))

/*
** ld3_vnum_u16_3:
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0, #3, mul vl\]
**	ret
*/
TEST_LOAD (ld3_vnum_u16_3, svuint16x3_t, uint16_t,
	   z0 = svld3_vnum_u16 (p0, x0, 3),
	   z0 = svld3_vnum (p0, x0, 3))

/*
** ld3_vnum_u16_21:
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0, #21, mul vl\]
**	ret
*/
TEST_LOAD (ld3_vnum_u16_21, svuint16x3_t, uint16_t,
	   z0 = svld3_vnum_u16 (p0, x0, 21),
	   z0 = svld3_vnum (p0, x0, 21))

/*
** ld3_vnum_u16_24:
**	addvl	(x[0-9]+), x0, #24
**	ld3h	{z0\.h - z2\.h}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld3_vnum_u16_24, svuint16x3_t, uint16_t,
	   z0 = svld3_vnum_u16 (p0, x0, 24),
	   z0 = svld3_vnum (p0, x0, 24))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_vnum_u16_m1:
**	decb	x0
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_vnum_u16_m1, svuint16x3_t, uint16_t,
	   z0 = svld3_vnum_u16 (p0, x0, -1),
	   z0 = svld3_vnum (p0, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_vnum_u16_m2:
**	decb	x0, all, mul #2
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_vnum_u16_m2, svuint16x3_t, uint16_t,
	   z0 = svld3_vnum_u16 (p0, x0, -2),
	   z0 = svld3_vnum (p0, x0, -2))

/*
** ld3_vnum_u16_m3:
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0, #-3, mul vl\]
**	ret
*/
TEST_LOAD (ld3_vnum_u16_m3, svuint16x3_t, uint16_t,
	   z0 = svld3_vnum_u16 (p0, x0, -3),
	   z0 = svld3_vnum (p0, x0, -3))

/*
** ld3_vnum_u16_m24:
**	ld3h	{z0\.h - z2\.h}, p0/z, \[x0, #-24, mul vl\]
**	ret
*/
TEST_LOAD (ld3_vnum_u16_m24, svuint16x3_t, uint16_t,
	   z0 = svld3_vnum_u16 (p0, x0, -24),
	   z0 = svld3_vnum (p0, x0, -24))

/*
** ld3_vnum_u16_m27:
**	addvl	(x[0-9]+), x0, #-27
**	ld3h	{z0\.h - z2\.h}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld3_vnum_u16_m27, svuint16x3_t, uint16_t,
	   z0 = svld3_vnum_u16 (p0, x0, -27),
	   z0 = svld3_vnum (p0, x0, -27))

/* Using MUL to calculate an index would also be OK.  */
/*
** ld3_vnum_u16_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ld3h	{z0\.h - z2\.h}, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ld3_vnum_u16_x1, svuint16x3_t, uint16_t,
	   z0 = svld3_vnum_u16 (p0, x0, x1),
	   z0 = svld3_vnum (p0, x0, x1))
