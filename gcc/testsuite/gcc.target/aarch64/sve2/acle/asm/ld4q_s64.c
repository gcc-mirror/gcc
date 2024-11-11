/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** ld4q_s64_base:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s64_base, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0),
	   z0 = svld4q (p0, x0))

/*
** ld4q_s64_index:
**	add	(x[0-9]), x0, x1, lsl #?3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld4q_s64_index, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 + x1),
	   z0 = svld4q (p0, x0 + x1))

/*
** ld4q_s64_index2:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_LOAD (ld4q_s64_index2, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 + x1 * 2),
	   z0 = svld4q (p0, x0 + x1 * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s64_1:
**	incb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s64_1, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 + svcntd ()),
	   z0 = svld4q (p0, x0 + svcntd ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s64_2:
**	incb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s64_2, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 + svcntd () * 2),
	   z0 = svld4q (p0, x0 + svcntd () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s64_3:
**	incb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s64_3, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 + svcntd () * 3),
	   z0 = svld4q (p0, x0 + svcntd () * 3))

/*
** ld4q_s64_4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_s64_4, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 + svcntd () * 4),
	   z0 = svld4q (p0, x0 + svcntd () * 4))

/*
** ld4q_s64_28:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_s64_28, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 + svcntd () * 28),
	   z0 = svld4q (p0, x0 + svcntd () * 28))

/*
** ld4q_s64_32:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_s64_32, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 + svcntd () * 32),
	   z0 = svld4q (p0, x0 + svcntd () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s64_m1:
**	decb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s64_m1, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 - svcntd ()),
	   z0 = svld4q (p0, x0 - svcntd ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s64_m2:
**	decb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s64_m2, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 - svcntd () * 2),
	   z0 = svld4q (p0, x0 - svcntd () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s64_m3:
**	decb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s64_m3, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 - svcntd () * 3),
	   z0 = svld4q (p0, x0 - svcntd () * 3))

/*
** ld4q_s64_m4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_s64_m4, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 - svcntd () * 4),
	   z0 = svld4q (p0, x0 - svcntd () * 4))

/*
** ld4q_s64_m32:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_s64_m32, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 - svcntd () * 32),
	   z0 = svld4q (p0, x0 - svcntd () * 32))

/*
** ld4q_s64_m36:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_s64_m36, svint64x4_t, int64_t,
	   z0 = svld4q_s64 (p0, x0 - svcntd () * 36),
	   z0 = svld4q (p0, x0 - svcntd () * 36))

/*
** ld4q_vnum_s64_0:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_0, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, 0),
	   z0 = svld4q_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s64_1:
**	incb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_1, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, 1),
	   z0 = svld4q_vnum (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s64_2:
**	incb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_2, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, 2),
	   z0 = svld4q_vnum (p0, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s64_3:
**	incb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_3, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, 3),
	   z0 = svld4q_vnum (p0, x0, 3))

/*
** ld4q_vnum_s64_4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_4, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, 4),
	   z0 = svld4q_vnum (p0, x0, 4))

/*
** ld4q_vnum_s64_28:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_28, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, 28),
	   z0 = svld4q_vnum (p0, x0, 28))

/*
** ld4q_vnum_s64_32:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_32, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, 32),
	   z0 = svld4q_vnum (p0, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s64_m1:
**	decb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_m1, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, -1),
	   z0 = svld4q_vnum (p0, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s64_m2:
**	decb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_m2, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, -2),
	   z0 = svld4q_vnum (p0, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s64_m3:
**	decb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_m3, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, -3),
	   z0 = svld4q_vnum (p0, x0, -3))

/*
** ld4q_vnum_s64_m4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_m4, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, -4),
	   z0 = svld4q_vnum (p0, x0, -4))

/*
** ld4q_vnum_s64_m32:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_m32, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, -32),
	   z0 = svld4q_vnum (p0, x0, -32))

/*
** ld4q_vnum_s64_m36:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_m36, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, -36),
	   z0 = svld4q_vnum (p0, x0, -36))

/*
** ld4q_vnum_s64_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD (ld4q_vnum_s64_x1, svint64x4_t, int64_t,
	   z0 = svld4q_vnum_s64 (p0, x0, x1),
	   z0 = svld4q_vnum (p0, x0, x1))
