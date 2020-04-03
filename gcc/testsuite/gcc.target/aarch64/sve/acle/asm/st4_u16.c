/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** st4_u16_base:
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_u16_base, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0, z0),
	    svst4 (p0, x0, z0))

/*
** st4_u16_index:
**	st4h	{z0\.h - z3\.h}, p0, \[x0, x1, lsl 1\]
**	ret
*/
TEST_STORE (st4_u16_index, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 + x1, z0),
	    svst4 (p0, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_u16_1:
**	incb	x0
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_u16_1, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 + svcnth (), z0),
	    svst4 (p0, x0 + svcnth (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_u16_2:
**	incb	x0, all, mul #2
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_u16_2, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 + svcnth () * 2, z0),
	    svst4 (p0, x0 + svcnth () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_u16_3:
**	incb	x0, all, mul #3
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_u16_3, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 + svcnth () * 3, z0),
	    svst4 (p0, x0 + svcnth () * 3, z0))

/*
** st4_u16_4:
**	st4h	{z0\.h - z3\.h}, p0, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE (st4_u16_4, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 + svcnth () * 4, z0),
	    svst4 (p0, x0 + svcnth () * 4, z0))

/*
** st4_u16_28:
**	st4h	{z0\.h - z3\.h}, p0, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE (st4_u16_28, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 + svcnth () * 28, z0),
	    svst4 (p0, x0 + svcnth () * 28, z0))

/*
** st4_u16_32:
**	[^{]*
**	st4h	{z0\.h - z3\.h}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4_u16_32, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 + svcnth () * 32, z0),
	    svst4 (p0, x0 + svcnth () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_u16_m1:
**	decb	x0
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_u16_m1, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 - svcnth (), z0),
	    svst4 (p0, x0 - svcnth (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_u16_m2:
**	decb	x0, all, mul #2
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_u16_m2, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 - svcnth () * 2, z0),
	    svst4 (p0, x0 - svcnth () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_u16_m3:
**	decb	x0, all, mul #3
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_u16_m3, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 - svcnth () * 3, z0),
	    svst4 (p0, x0 - svcnth () * 3, z0))

/*
** st4_u16_m4:
**	st4h	{z0\.h - z3\.h}, p0, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE (st4_u16_m4, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 - svcnth () * 4, z0),
	    svst4 (p0, x0 - svcnth () * 4, z0))

/*
** st4_u16_m32:
**	st4h	{z0\.h - z3\.h}, p0, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE (st4_u16_m32, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 - svcnth () * 32, z0),
	    svst4 (p0, x0 - svcnth () * 32, z0))

/*
** st4_u16_m36:
**	[^{]*
**	st4h	{z0\.h - z3\.h}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4_u16_m36, svuint16x4_t, uint16_t,
	    svst4_u16 (p0, x0 - svcnth () * 36, z0),
	    svst4 (p0, x0 - svcnth () * 36, z0))

/*
** st4_vnum_u16_0:
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_u16_0, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, 0, z0),
	    svst4_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_u16_1:
**	incb	x0
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_u16_1, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, 1, z0),
	    svst4_vnum (p0, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_u16_2:
**	incb	x0, all, mul #2
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_u16_2, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, 2, z0),
	    svst4_vnum (p0, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_u16_3:
**	incb	x0, all, mul #3
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_u16_3, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, 3, z0),
	    svst4_vnum (p0, x0, 3, z0))

/*
** st4_vnum_u16_4:
**	st4h	{z0\.h - z3\.h}, p0, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE (st4_vnum_u16_4, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, 4, z0),
	    svst4_vnum (p0, x0, 4, z0))

/*
** st4_vnum_u16_28:
**	st4h	{z0\.h - z3\.h}, p0, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE (st4_vnum_u16_28, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, 28, z0),
	    svst4_vnum (p0, x0, 28, z0))

/*
** st4_vnum_u16_32:
**	[^{]*
**	st4h	{z0\.h - z3\.h}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4_vnum_u16_32, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, 32, z0),
	    svst4_vnum (p0, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_u16_m1:
**	decb	x0
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_u16_m1, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, -1, z0),
	    svst4_vnum (p0, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_u16_m2:
**	decb	x0, all, mul #2
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_u16_m2, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, -2, z0),
	    svst4_vnum (p0, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_u16_m3:
**	decb	x0, all, mul #3
**	st4h	{z0\.h - z3\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_u16_m3, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, -3, z0),
	    svst4_vnum (p0, x0, -3, z0))

/*
** st4_vnum_u16_m4:
**	st4h	{z0\.h - z3\.h}, p0, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE (st4_vnum_u16_m4, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, -4, z0),
	    svst4_vnum (p0, x0, -4, z0))

/*
** st4_vnum_u16_m32:
**	st4h	{z0\.h - z3\.h}, p0, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE (st4_vnum_u16_m32, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, -32, z0),
	    svst4_vnum (p0, x0, -32, z0))

/*
** st4_vnum_u16_m36:
**	[^{]*
**	st4h	{z0\.h - z3\.h}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4_vnum_u16_m36, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, -36, z0),
	    svst4_vnum (p0, x0, -36, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** st4_vnum_u16_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	st4h	{z0\.h - z3\.h}, p0, \[\2\]
**	ret
*/
TEST_STORE (st4_vnum_u16_x1, svuint16x4_t, uint16_t,
	    svst4_vnum_u16 (p0, x0, x1, z0),
	    svst4_vnum (p0, x0, x1, z0))
