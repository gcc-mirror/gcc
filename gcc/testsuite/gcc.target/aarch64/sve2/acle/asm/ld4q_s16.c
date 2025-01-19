/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** ld4q_s16_base:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s16_base, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0),
	   z0 = svld4q (p0, x0))

/*
** ld4q_s16_index:
**	add	(x[0-9]), x0, x1, lsl #?1
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld4q_s16_index, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 + x1),
	   z0 = svld4q (p0, x0 + x1))

/*
** ld4q_s16_index2:
**	add	(x[0-9]), x0, x1, lsl #?2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld4q_s16_index2, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 + x1 * 2),
	   z0 = svld4q (p0, x0 + x1 * 2))

/*
** ld4q_s16_index4:
**	add	(x[0-9]), x0, x1, lsl #?3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld4q_s16_index4, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 + x1 * 4),
	   z0 = svld4q (p0, x0 + x1 * 4))

/*
** ld4q_s16_index8:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_LOAD (ld4q_s16_index8, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 + x1 * 8),
	   z0 = svld4q (p0, x0 + x1 * 8))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s16_1:
**	incb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s16_1, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 + svcnth ()),
	   z0 = svld4q (p0, x0 + svcnth ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s16_2:
**	incb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s16_2, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 + svcnth () * 2),
	   z0 = svld4q (p0, x0 + svcnth () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s16_3:
**	incb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s16_3, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 + svcnth () * 3),
	   z0 = svld4q (p0, x0 + svcnth () * 3))

/*
** ld4q_s16_4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_s16_4, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 + svcnth () * 4),
	   z0 = svld4q (p0, x0 + svcnth () * 4))

/*
** ld4q_s16_28:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_s16_28, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 + svcnth () * 28),
	   z0 = svld4q (p0, x0 + svcnth () * 28))

/*
** ld4q_s16_32:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_s16_32, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 + svcnth () * 32),
	   z0 = svld4q (p0, x0 + svcnth () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s16_m1:
**	decb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s16_m1, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 - svcnth ()),
	   z0 = svld4q (p0, x0 - svcnth ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s16_m2:
**	decb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s16_m2, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 - svcnth () * 2),
	   z0 = svld4q (p0, x0 - svcnth () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s16_m3:
**	decb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s16_m3, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 - svcnth () * 3),
	   z0 = svld4q (p0, x0 - svcnth () * 3))

/*
** ld4q_s16_m4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_s16_m4, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 - svcnth () * 4),
	   z0 = svld4q (p0, x0 - svcnth () * 4))

/*
** ld4q_s16_m32:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_s16_m32, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 - svcnth () * 32),
	   z0 = svld4q (p0, x0 - svcnth () * 32))

/*
** ld4q_s16_m36:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_s16_m36, svint16x4_t, int16_t,
	   z0 = svld4q_s16 (p0, x0 - svcnth () * 36),
	   z0 = svld4q (p0, x0 - svcnth () * 36))

/*
** ld4q_vnum_s16_0:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_0, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, 0),
	   z0 = svld4q_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s16_1:
**	incb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_1, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, 1),
	   z0 = svld4q_vnum (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s16_2:
**	incb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_2, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, 2),
	   z0 = svld4q_vnum (p0, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s16_3:
**	incb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_3, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, 3),
	   z0 = svld4q_vnum (p0, x0, 3))

/*
** ld4q_vnum_s16_4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_4, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, 4),
	   z0 = svld4q_vnum (p0, x0, 4))

/*
** ld4q_vnum_s16_28:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_28, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, 28),
	   z0 = svld4q_vnum (p0, x0, 28))

/*
** ld4q_vnum_s16_32:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_32, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, 32),
	   z0 = svld4q_vnum (p0, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s16_m1:
**	decb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_m1, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, -1),
	   z0 = svld4q_vnum (p0, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s16_m2:
**	decb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_m2, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, -2),
	   z0 = svld4q_vnum (p0, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s16_m3:
**	decb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_m3, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, -3),
	   z0 = svld4q_vnum (p0, x0, -3))

/*
** ld4q_vnum_s16_m4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_m4, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, -4),
	   z0 = svld4q_vnum (p0, x0, -4))

/*
** ld4q_vnum_s16_m32:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_m32, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, -32),
	   z0 = svld4q_vnum (p0, x0, -32))

/*
** ld4q_vnum_s16_m36:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s16_m36, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, -36),
	   z0 = svld4q_vnum (p0, x0, -36))

/*
** ld4q_vnum_s16_x1:
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
TEST_LOAD (ld4q_vnum_s16_x1, svint16x4_t, int16_t,
	   z0 = svld4q_vnum_s16 (p0, x0, x1),
	   z0 = svld4q_vnum (p0, x0, x1))
