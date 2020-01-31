/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** st2_bf16_base:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_bf16_base, svbfloat16x2_t, bfloat16_t,
	    svst2_bf16 (p0, x0, z0),
	    svst2 (p0, x0, z0))

/*
** st2_bf16_index:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, x1, lsl 1\]
**	ret
*/
TEST_STORE (st2_bf16_index, svbfloat16x2_t, bfloat16_t,
	    svst2_bf16 (p0, x0 + x1, z0),
	    svst2 (p0, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_bf16_1:
**	incb	x0
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_bf16_1, svbfloat16x2_t, bfloat16_t,
	    svst2_bf16 (p0, x0 + svcnth (), z0),
	    svst2 (p0, x0 + svcnth (), z0))

/*
** st2_bf16_2:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2_bf16_2, svbfloat16x2_t, bfloat16_t,
	    svst2_bf16 (p0, x0 + svcnth () * 2, z0),
	    svst2 (p0, x0 + svcnth () * 2, z0))

/*
** st2_bf16_14:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2_bf16_14, svbfloat16x2_t, bfloat16_t,
	    svst2_bf16 (p0, x0 + svcnth () * 14, z0),
	    svst2 (p0, x0 + svcnth () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_bf16_16:
**	incb	x0, all, mul #16
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_bf16_16, svbfloat16x2_t, bfloat16_t,
	    svst2_bf16 (p0, x0 + svcnth () * 16, z0),
	    svst2 (p0, x0 + svcnth () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_bf16_m1:
**	decb	x0
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_bf16_m1, svbfloat16x2_t, bfloat16_t,
	    svst2_bf16 (p0, x0 - svcnth (), z0),
	    svst2 (p0, x0 - svcnth (), z0))

/*
** st2_bf16_m2:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2_bf16_m2, svbfloat16x2_t, bfloat16_t,
	    svst2_bf16 (p0, x0 - svcnth () * 2, z0),
	    svst2 (p0, x0 - svcnth () * 2, z0))

/*
** st2_bf16_m16:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2_bf16_m16, svbfloat16x2_t, bfloat16_t,
	    svst2_bf16 (p0, x0 - svcnth () * 16, z0),
	    svst2 (p0, x0 - svcnth () * 16, z0))

/*
** st2_bf16_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2_bf16_m18, svbfloat16x2_t, bfloat16_t,
	    svst2_bf16 (p0, x0 - svcnth () * 18, z0),
	    svst2 (p0, x0 - svcnth () * 18, z0))

/*
** st2_vnum_bf16_0:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_bf16_0, svbfloat16x2_t, bfloat16_t,
	    svst2_vnum_bf16 (p0, x0, 0, z0),
	    svst2_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_bf16_1:
**	incb	x0
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_bf16_1, svbfloat16x2_t, bfloat16_t,
	    svst2_vnum_bf16 (p0, x0, 1, z0),
	    svst2_vnum (p0, x0, 1, z0))

/*
** st2_vnum_bf16_2:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_bf16_2, svbfloat16x2_t, bfloat16_t,
	    svst2_vnum_bf16 (p0, x0, 2, z0),
	    svst2_vnum (p0, x0, 2, z0))

/*
** st2_vnum_bf16_14:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_bf16_14, svbfloat16x2_t, bfloat16_t,
	    svst2_vnum_bf16 (p0, x0, 14, z0),
	    svst2_vnum (p0, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_bf16_16:
**	incb	x0, all, mul #16
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_bf16_16, svbfloat16x2_t, bfloat16_t,
	    svst2_vnum_bf16 (p0, x0, 16, z0),
	    svst2_vnum (p0, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_bf16_m1:
**	decb	x0
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_bf16_m1, svbfloat16x2_t, bfloat16_t,
	    svst2_vnum_bf16 (p0, x0, -1, z0),
	    svst2_vnum (p0, x0, -1, z0))

/*
** st2_vnum_bf16_m2:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_bf16_m2, svbfloat16x2_t, bfloat16_t,
	    svst2_vnum_bf16 (p0, x0, -2, z0),
	    svst2_vnum (p0, x0, -2, z0))

/*
** st2_vnum_bf16_m16:
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_bf16_m16, svbfloat16x2_t, bfloat16_t,
	    svst2_vnum_bf16 (p0, x0, -16, z0),
	    svst2_vnum (p0, x0, -16, z0))

/*
** st2_vnum_bf16_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2_vnum_bf16_m18, svbfloat16x2_t, bfloat16_t,
	    svst2_vnum_bf16 (p0, x0, -18, z0),
	    svst2_vnum (p0, x0, -18, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** st2_vnum_bf16_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	st2h	{z0\.h(?: - |, )z1\.h}, p0, \[\2\]
**	ret
*/
TEST_STORE (st2_vnum_bf16_x1, svbfloat16x2_t, bfloat16_t,
	    svst2_vnum_bf16 (p0, x0, x1, z0),
	    svst2_vnum (p0, x0, x1, z0))
