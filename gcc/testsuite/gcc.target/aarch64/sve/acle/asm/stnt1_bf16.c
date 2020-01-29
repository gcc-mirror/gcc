/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** stnt1_bf16_base:
**	stnt1h	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_bf16_base, svbfloat16_t, bfloat16_t,
	    svstnt1_bf16 (p0, x0, z0),
	    svstnt1 (p0, x0, z0))

/*
** stnt1_bf16_index:
**	stnt1h	z0\.h, p0, \[x0, x1, lsl 1\]
**	ret
*/
TEST_STORE (stnt1_bf16_index, svbfloat16_t, bfloat16_t,
	    svstnt1_bf16 (p0, x0 + x1, z0),
	    svstnt1 (p0, x0 + x1, z0))

/*
** stnt1_bf16_1:
**	stnt1h	z0\.h, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (stnt1_bf16_1, svbfloat16_t, bfloat16_t,
	    svstnt1_bf16 (p0, x0 + svcnth (), z0),
	    svstnt1 (p0, x0 + svcnth (), z0))

/*
** stnt1_bf16_7:
**	stnt1h	z0\.h, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (stnt1_bf16_7, svbfloat16_t, bfloat16_t,
	    svstnt1_bf16 (p0, x0 + svcnth () * 7, z0),
	    svstnt1 (p0, x0 + svcnth () * 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_bf16_8:
**	incb	x0, all, mul #8
**	stnt1h	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_bf16_8, svbfloat16_t, bfloat16_t,
	    svstnt1_bf16 (p0, x0 + svcnth () * 8, z0),
	    svstnt1 (p0, x0 + svcnth () * 8, z0))

/*
** stnt1_bf16_m1:
**	stnt1h	z0\.h, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (stnt1_bf16_m1, svbfloat16_t, bfloat16_t,
	    svstnt1_bf16 (p0, x0 - svcnth (), z0),
	    svstnt1 (p0, x0 - svcnth (), z0))

/*
** stnt1_bf16_m8:
**	stnt1h	z0\.h, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (stnt1_bf16_m8, svbfloat16_t, bfloat16_t,
	    svstnt1_bf16 (p0, x0 - svcnth () * 8, z0),
	    svstnt1 (p0, x0 - svcnth () * 8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_bf16_m9:
**	decb	x0, all, mul #9
**	stnt1h	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_bf16_m9, svbfloat16_t, bfloat16_t,
	    svstnt1_bf16 (p0, x0 - svcnth () * 9, z0),
	    svstnt1 (p0, x0 - svcnth () * 9, z0))

/*
** stnt1_vnum_bf16_0:
**	stnt1h	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_vnum_bf16_0, svbfloat16_t, bfloat16_t,
	    svstnt1_vnum_bf16 (p0, x0, 0, z0),
	    svstnt1_vnum (p0, x0, 0, z0))

/*
** stnt1_vnum_bf16_1:
**	stnt1h	z0\.h, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (stnt1_vnum_bf16_1, svbfloat16_t, bfloat16_t,
	    svstnt1_vnum_bf16 (p0, x0, 1, z0),
	    svstnt1_vnum (p0, x0, 1, z0))

/*
** stnt1_vnum_bf16_7:
**	stnt1h	z0\.h, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (stnt1_vnum_bf16_7, svbfloat16_t, bfloat16_t,
	    svstnt1_vnum_bf16 (p0, x0, 7, z0),
	    svstnt1_vnum (p0, x0, 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_bf16_8:
**	incb	x0, all, mul #8
**	stnt1h	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_vnum_bf16_8, svbfloat16_t, bfloat16_t,
	    svstnt1_vnum_bf16 (p0, x0, 8, z0),
	    svstnt1_vnum (p0, x0, 8, z0))

/*
** stnt1_vnum_bf16_m1:
**	stnt1h	z0\.h, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (stnt1_vnum_bf16_m1, svbfloat16_t, bfloat16_t,
	    svstnt1_vnum_bf16 (p0, x0, -1, z0),
	    svstnt1_vnum (p0, x0, -1, z0))

/*
** stnt1_vnum_bf16_m8:
**	stnt1h	z0\.h, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (stnt1_vnum_bf16_m8, svbfloat16_t, bfloat16_t,
	    svstnt1_vnum_bf16 (p0, x0, -8, z0),
	    svstnt1_vnum (p0, x0, -8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_bf16_m9:
**	decb	x0, all, mul #9
**	stnt1h	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_vnum_bf16_m9, svbfloat16_t, bfloat16_t,
	    svstnt1_vnum_bf16 (p0, x0, -9, z0),
	    svstnt1_vnum (p0, x0, -9, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** stnt1_vnum_bf16_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	stnt1h	z0\.h, p0, \[\2\]
**	ret
*/
TEST_STORE (stnt1_vnum_bf16_x1, svbfloat16_t, bfloat16_t,
	    svstnt1_vnum_bf16 (p0, x0, x1, z0),
	    svstnt1_vnum (p0, x0, x1, z0))
