/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st2_u16_base:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_u16_base, svuint16x2_t, uint16_t,
	    svst2_u16 (p0, x0, z0),
	    svst2 (p0, x0, z0))

/*
** st2_u16_index:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, x1, lsl 1\]
**	ret
*/
TEST_STORE (st2_u16_index, svuint16x2_t, uint16_t,
	    svst2_u16 (p0, x0 + x1, z0),
	    svst2 (p0, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_u16_1:
**	incb	x0
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_u16_1, svuint16x2_t, uint16_t,
	    svst2_u16 (p0, x0 + svcnth (), z0),
	    svst2 (p0, x0 + svcnth (), z0))

/*
** st2_u16_2:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2_u16_2, svuint16x2_t, uint16_t,
	    svst2_u16 (p0, x0 + svcnth () * 2, z0),
	    svst2 (p0, x0 + svcnth () * 2, z0))

/*
** st2_u16_14:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2_u16_14, svuint16x2_t, uint16_t,
	    svst2_u16 (p0, x0 + svcnth () * 14, z0),
	    svst2 (p0, x0 + svcnth () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_u16_16:
**	incb	x0, all, mul #16
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_u16_16, svuint16x2_t, uint16_t,
	    svst2_u16 (p0, x0 + svcnth () * 16, z0),
	    svst2 (p0, x0 + svcnth () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_u16_m1:
**	decb	x0
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_u16_m1, svuint16x2_t, uint16_t,
	    svst2_u16 (p0, x0 - svcnth (), z0),
	    svst2 (p0, x0 - svcnth (), z0))

/*
** st2_u16_m2:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2_u16_m2, svuint16x2_t, uint16_t,
	    svst2_u16 (p0, x0 - svcnth () * 2, z0),
	    svst2 (p0, x0 - svcnth () * 2, z0))

/*
** st2_u16_m16:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2_u16_m16, svuint16x2_t, uint16_t,
	    svst2_u16 (p0, x0 - svcnth () * 16, z0),
	    svst2 (p0, x0 - svcnth () * 16, z0))

/*
** st2_u16_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2_u16_m18, svuint16x2_t, uint16_t,
	    svst2_u16 (p0, x0 - svcnth () * 18, z0),
	    svst2 (p0, x0 - svcnth () * 18, z0))

/*
** st2_vnum_u16_0:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_u16_0, svuint16x2_t, uint16_t,
	    svst2_vnum_u16 (p0, x0, 0, z0),
	    svst2_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_u16_1:
**	incb	x0
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_u16_1, svuint16x2_t, uint16_t,
	    svst2_vnum_u16 (p0, x0, 1, z0),
	    svst2_vnum (p0, x0, 1, z0))

/*
** st2_vnum_u16_2:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_u16_2, svuint16x2_t, uint16_t,
	    svst2_vnum_u16 (p0, x0, 2, z0),
	    svst2_vnum (p0, x0, 2, z0))

/*
** st2_vnum_u16_14:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_u16_14, svuint16x2_t, uint16_t,
	    svst2_vnum_u16 (p0, x0, 14, z0),
	    svst2_vnum (p0, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_u16_16:
**	incb	x0, all, mul #16
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_u16_16, svuint16x2_t, uint16_t,
	    svst2_vnum_u16 (p0, x0, 16, z0),
	    svst2_vnum (p0, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_u16_m1:
**	decb	x0
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_u16_m1, svuint16x2_t, uint16_t,
	    svst2_vnum_u16 (p0, x0, -1, z0),
	    svst2_vnum (p0, x0, -1, z0))

/*
** st2_vnum_u16_m2:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_u16_m2, svuint16x2_t, uint16_t,
	    svst2_vnum_u16 (p0, x0, -2, z0),
	    svst2_vnum (p0, x0, -2, z0))

/*
** st2_vnum_u16_m16:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_u16_m16, svuint16x2_t, uint16_t,
	    svst2_vnum_u16 (p0, x0, -16, z0),
	    svst2_vnum (p0, x0, -16, z0))

/*
** st2_vnum_u16_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2_vnum_u16_m18, svuint16x2_t, uint16_t,
	    svst2_vnum_u16 (p0, x0, -18, z0),
	    svst2_vnum (p0, x0, -18, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** st2_vnum_u16_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[\2\]
**	ret
*/
TEST_STORE (st2_vnum_u16_x1, svuint16x2_t, uint16_t,
	    svst2_vnum_u16 (p0, x0, x1, z0),
	    svst2_vnum (p0, x0, x1, z0))
