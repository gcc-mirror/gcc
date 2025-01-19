/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** st4q_bf16_base:
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_bf16_base, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0, z0),
	    svst4q (p0, x0, z0))

/*
** st4q_bf16_index:
**	add	(x[0-9]), x0, x1, lsl #?1
**	st4q	{z0\.q - z3\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st4q_bf16_index, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 + x1, z0),
	    svst4q (p0, x0 + x1, z0))

/*
** st4q_bf16_index2:
**	add	(x[0-9]), x0, x1, lsl #?2
**	st4q	{z0\.q - z3\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st4q_bf16_index2, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 + x1 * 2, z0),
	    svst4q (p0, x0 + x1 * 2, z0))

/*
** st4q_bf16_index4:
**	add	(x[0-9]), x0, x1, lsl #?3
**	st4q	{z0\.q - z3\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st4q_bf16_index4, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 + x1 * 4, z0),
	    svst4q (p0, x0 + x1 * 4, z0))

/*
** st4q_bf16_index8:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_STORE (st4q_bf16_index8, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 + x1 * 8, z0),
	    svst4q (p0, x0 + x1 * 8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_bf16_1:
**	incb	x0
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_bf16_1, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 + svcnth (), z0),
	    svst4q (p0, x0 + svcnth (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_bf16_2:
**	incb	x0, all, mul #2
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_bf16_2, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 + svcnth () * 2, z0),
	    svst4q (p0, x0 + svcnth () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_bf16_3:
**	incb	x0, all, mul #3
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_bf16_3, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 + svcnth () * 3, z0),
	    svst4q (p0, x0 + svcnth () * 3, z0))

/*
** st4q_bf16_4:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE (st4q_bf16_4, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 + svcnth () * 4, z0),
	    svst4q (p0, x0 + svcnth () * 4, z0))

/*
** st4q_bf16_28:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE (st4q_bf16_28, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 + svcnth () * 28, z0),
	    svst4q (p0, x0 + svcnth () * 28, z0))

/*
** st4q_bf16_32:
**	[^{]*
**	st4q	{z0\.q - z3\.q}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4q_bf16_32, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 + svcnth () * 32, z0),
	    svst4q (p0, x0 + svcnth () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_bf16_m1:
**	decb	x0
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_bf16_m1, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 - svcnth (), z0),
	    svst4q (p0, x0 - svcnth (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_bf16_m2:
**	decb	x0, all, mul #2
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_bf16_m2, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 - svcnth () * 2, z0),
	    svst4q (p0, x0 - svcnth () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_bf16_m3:
**	decb	x0, all, mul #3
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_bf16_m3, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 - svcnth () * 3, z0),
	    svst4q (p0, x0 - svcnth () * 3, z0))

/*
** st4q_bf16_m4:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE (st4q_bf16_m4, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 - svcnth () * 4, z0),
	    svst4q (p0, x0 - svcnth () * 4, z0))

/*
** st4q_bf16_m32:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE (st4q_bf16_m32, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 - svcnth () * 32, z0),
	    svst4q (p0, x0 - svcnth () * 32, z0))

/*
** st4q_bf16_m36:
**	[^{]*
**	st4q	{z0\.q - z3\.q}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4q_bf16_m36, svbfloat16x4_t, bfloat16_t,
	    svst4q_bf16 (p0, x0 - svcnth () * 36, z0),
	    svst4q (p0, x0 - svcnth () * 36, z0))

/*
** st4q_vnum_bf16_0:
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_0, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, 0, z0),
	    svst4q_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_vnum_bf16_1:
**	incb	x0
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_1, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, 1, z0),
	    svst4q_vnum (p0, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_vnum_bf16_2:
**	incb	x0, all, mul #2
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_2, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, 2, z0),
	    svst4q_vnum (p0, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_vnum_bf16_3:
**	incb	x0, all, mul #3
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_3, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, 3, z0),
	    svst4q_vnum (p0, x0, 3, z0))

/*
** st4q_vnum_bf16_4:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_4, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, 4, z0),
	    svst4q_vnum (p0, x0, 4, z0))

/*
** st4q_vnum_bf16_28:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_28, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, 28, z0),
	    svst4q_vnum (p0, x0, 28, z0))

/*
** st4q_vnum_bf16_32:
**	[^{]*
**	st4q	{z0\.q - z3\.q}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_32, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, 32, z0),
	    svst4q_vnum (p0, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_vnum_bf16_m1:
**	decb	x0
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_m1, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, -1, z0),
	    svst4q_vnum (p0, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_vnum_bf16_m2:
**	decb	x0, all, mul #2
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_m2, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, -2, z0),
	    svst4q_vnum (p0, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_vnum_bf16_m3:
**	decb	x0, all, mul #3
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_m3, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, -3, z0),
	    svst4q_vnum (p0, x0, -3, z0))

/*
** st4q_vnum_bf16_m4:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_m4, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, -4, z0),
	    svst4q_vnum (p0, x0, -4, z0))

/*
** st4q_vnum_bf16_m32:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_m32, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, -32, z0),
	    svst4q_vnum (p0, x0, -32, z0))

/*
** st4q_vnum_bf16_m36:
**	[^{]*
**	st4q	{z0\.q - z3\.q}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4q_vnum_bf16_m36, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, -36, z0),
	    svst4q_vnum (p0, x0, -36, z0))

/*
** st4q_vnum_bf16_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st4q	{z0\.q - z3\.q}, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st4q	{z0\.q - z3\.q}, p0, \[x0, \3\]
** )
**	ret
*/
TEST_STORE (st4q_vnum_bf16_x1, svbfloat16x4_t, bfloat16_t,
	    svst4q_vnum_bf16 (p0, x0, x1, z0),
	    svst4q_vnum (p0, x0, x1, z0))
