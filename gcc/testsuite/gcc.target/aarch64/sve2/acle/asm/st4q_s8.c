/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** st4q_s8_base:
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_s8_base, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0, z0),
	    svst4q (p0, x0, z0))

/*
** st4q_s8_index:
**	add	(x[0-9]), (?:x0, x1|x1, x0)
**	st4q	{z0\.q - z3\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st4q_s8_index, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 + x1, z0),
	    svst4q (p0, x0 + x1, z0))

/*
** st4q_s8_index2:
**	add	(x[0-9]), x0, x1, lsl #?1
**	st4q	{z0\.q - z3\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st4q_s8_index2, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 + x1 * 2, z0),
	    svst4q (p0, x0 + x1 * 2, z0))

/*
** st4q_s8_index4:
**	add	(x[0-9]), x0, x1, lsl #?2
**	st4q	{z0\.q - z3\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st4q_s8_index4, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 + x1 * 4, z0),
	    svst4q (p0, x0 + x1 * 4, z0))

/*
** st4q_s8_index8:
**	add	(x[0-9]), x0, x1, lsl #?3
**	st4q	{z0\.q - z3\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st4q_s8_index8, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 + x1 * 8, z0),
	    svst4q (p0, x0 + x1 * 8, z0))

/*
** st4q_s8_index16:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_STORE (st4q_s8_index16, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 + x1 * 16, z0),
	    svst4q (p0, x0 + x1 * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_s8_1:
**	incb	x0
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_s8_1, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 + svcntb (), z0),
	    svst4q (p0, x0 + svcntb (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_s8_2:
**	incb	x0, all, mul #2
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_s8_2, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 + svcntb () * 2, z0),
	    svst4q (p0, x0 + svcntb () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_s8_3:
**	incb	x0, all, mul #3
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_s8_3, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 + svcntb () * 3, z0),
	    svst4q (p0, x0 + svcntb () * 3, z0))

/*
** st4q_s8_4:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE (st4q_s8_4, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 + svcntb () * 4, z0),
	    svst4q (p0, x0 + svcntb () * 4, z0))

/*
** st4q_s8_28:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE (st4q_s8_28, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 + svcntb () * 28, z0),
	    svst4q (p0, x0 + svcntb () * 28, z0))

/*
** st4q_s8_32:
**	[^{]*
**	st4q	{z0\.q - z3\.q}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4q_s8_32, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 + svcntb () * 32, z0),
	    svst4q (p0, x0 + svcntb () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_s8_m1:
**	decb	x0
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_s8_m1, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 - svcntb (), z0),
	    svst4q (p0, x0 - svcntb (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_s8_m2:
**	decb	x0, all, mul #2
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_s8_m2, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 - svcntb () * 2, z0),
	    svst4q (p0, x0 - svcntb () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_s8_m3:
**	decb	x0, all, mul #3
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_s8_m3, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 - svcntb () * 3, z0),
	    svst4q (p0, x0 - svcntb () * 3, z0))

/*
** st4q_s8_m4:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE (st4q_s8_m4, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 - svcntb () * 4, z0),
	    svst4q (p0, x0 - svcntb () * 4, z0))

/*
** st4q_s8_m32:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE (st4q_s8_m32, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 - svcntb () * 32, z0),
	    svst4q (p0, x0 - svcntb () * 32, z0))

/*
** st4q_s8_m36:
**	[^{]*
**	st4q	{z0\.q - z3\.q}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4q_s8_m36, svint8x4_t, int8_t,
	    svst4q_s8 (p0, x0 - svcntb () * 36, z0),
	    svst4q (p0, x0 - svcntb () * 36, z0))

/*
** st4q_vnum_s8_0:
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_0, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, 0, z0),
	    svst4q_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_vnum_s8_1:
**	incb	x0
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_1, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, 1, z0),
	    svst4q_vnum (p0, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_vnum_s8_2:
**	incb	x0, all, mul #2
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_2, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, 2, z0),
	    svst4q_vnum (p0, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_vnum_s8_3:
**	incb	x0, all, mul #3
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_3, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, 3, z0),
	    svst4q_vnum (p0, x0, 3, z0))

/*
** st4q_vnum_s8_4:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_4, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, 4, z0),
	    svst4q_vnum (p0, x0, 4, z0))

/*
** st4q_vnum_s8_28:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_28, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, 28, z0),
	    svst4q_vnum (p0, x0, 28, z0))

/*
** st4q_vnum_s8_32:
**	[^{]*
**	st4q	{z0\.q - z3\.q}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_32, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, 32, z0),
	    svst4q_vnum (p0, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_vnum_s8_m1:
**	decb	x0
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_m1, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, -1, z0),
	    svst4q_vnum (p0, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_vnum_s8_m2:
**	decb	x0, all, mul #2
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_m2, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, -2, z0),
	    svst4q_vnum (p0, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4q_vnum_s8_m3:
**	decb	x0, all, mul #3
**	st4q	{z0\.q - z3\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_m3, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, -3, z0),
	    svst4q_vnum (p0, x0, -3, z0))

/*
** st4q_vnum_s8_m4:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_m4, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, -4, z0),
	    svst4q_vnum (p0, x0, -4, z0))

/*
** st4q_vnum_s8_m32:
**	st4q	{z0\.q - z3\.q}, p0, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_m32, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, -32, z0),
	    svst4q_vnum (p0, x0, -32, z0))

/*
** st4q_vnum_s8_m36:
**	[^{]*
**	st4q	{z0\.q - z3\.q}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4q_vnum_s8_m36, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, -36, z0),
	    svst4q_vnum (p0, x0, -36, z0))

/*
** st4q_vnum_s8_x1:
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
TEST_STORE (st4q_vnum_s8_x1, svint8x4_t, int8_t,
	    svst4q_vnum_s8 (p0, x0, x1, z0),
	    svst4q_vnum (p0, x0, x1, z0))
