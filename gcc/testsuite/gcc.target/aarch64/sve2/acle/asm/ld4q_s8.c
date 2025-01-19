/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** ld4q_s8_base:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s8_base, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0),
	   z0 = svld4q (p0, x0))

/*
** ld4q_s8_index:
**	add	(x[0-9]), (?:x0, x1|x1, x0)
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld4q_s8_index, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 + x1),
	   z0 = svld4q (p0, x0 + x1))

/*
** ld4q_s8_index2:
**	add	(x[0-9]), x0, x1, lsl #?1
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld4q_s8_index2, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 + x1 * 2),
	   z0 = svld4q (p0, x0 + x1 * 2))

/*
** ld4q_s8_index4:
**	add	(x[0-9]), x0, x1, lsl #?2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld4q_s8_index4, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 + x1 * 4),
	   z0 = svld4q (p0, x0 + x1 * 4))

/*
** ld4q_s8_index8:
**	add	(x[0-9]), x0, x1, lsl #?3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld4q_s8_index8, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 + x1 * 8),
	   z0 = svld4q (p0, x0 + x1 * 8))

/*
** ld4q_s8_index16:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_LOAD (ld4q_s8_index16, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 + x1 * 16),
	   z0 = svld4q (p0, x0 + x1 * 16))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s8_1:
**	incb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s8_1, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 + svcntb ()),
	   z0 = svld4q (p0, x0 + svcntb ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s8_2:
**	incb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s8_2, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 + svcntb () * 2),
	   z0 = svld4q (p0, x0 + svcntb () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s8_3:
**	incb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s8_3, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 + svcntb () * 3),
	   z0 = svld4q (p0, x0 + svcntb () * 3))

/*
** ld4q_s8_4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_s8_4, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 + svcntb () * 4),
	   z0 = svld4q (p0, x0 + svcntb () * 4))

/*
** ld4q_s8_28:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_s8_28, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 + svcntb () * 28),
	   z0 = svld4q (p0, x0 + svcntb () * 28))

/*
** ld4q_s8_32:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_s8_32, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 + svcntb () * 32),
	   z0 = svld4q (p0, x0 + svcntb () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s8_m1:
**	decb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s8_m1, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 - svcntb ()),
	   z0 = svld4q (p0, x0 - svcntb ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s8_m2:
**	decb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s8_m2, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 - svcntb () * 2),
	   z0 = svld4q (p0, x0 - svcntb () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_s8_m3:
**	decb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_s8_m3, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 - svcntb () * 3),
	   z0 = svld4q (p0, x0 - svcntb () * 3))

/*
** ld4q_s8_m4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_s8_m4, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 - svcntb () * 4),
	   z0 = svld4q (p0, x0 - svcntb () * 4))

/*
** ld4q_s8_m32:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_s8_m32, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 - svcntb () * 32),
	   z0 = svld4q (p0, x0 - svcntb () * 32))

/*
** ld4q_s8_m36:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_s8_m36, svint8x4_t, int8_t,
	   z0 = svld4q_s8 (p0, x0 - svcntb () * 36),
	   z0 = svld4q (p0, x0 - svcntb () * 36))

/*
** ld4q_vnum_s8_0:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_0, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, 0),
	   z0 = svld4q_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s8_1:
**	incb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_1, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, 1),
	   z0 = svld4q_vnum (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s8_2:
**	incb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_2, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, 2),
	   z0 = svld4q_vnum (p0, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s8_3:
**	incb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_3, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, 3),
	   z0 = svld4q_vnum (p0, x0, 3))

/*
** ld4q_vnum_s8_4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_4, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, 4),
	   z0 = svld4q_vnum (p0, x0, 4))

/*
** ld4q_vnum_s8_28:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_28, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, 28),
	   z0 = svld4q_vnum (p0, x0, 28))

/*
** ld4q_vnum_s8_32:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_32, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, 32),
	   z0 = svld4q_vnum (p0, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s8_m1:
**	decb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_m1, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, -1),
	   z0 = svld4q_vnum (p0, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s8_m2:
**	decb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_m2, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, -2),
	   z0 = svld4q_vnum (p0, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_s8_m3:
**	decb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_m3, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, -3),
	   z0 = svld4q_vnum (p0, x0, -3))

/*
** ld4q_vnum_s8_m4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_m4, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, -4),
	   z0 = svld4q_vnum (p0, x0, -4))

/*
** ld4q_vnum_s8_m32:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_m32, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, -32),
	   z0 = svld4q_vnum (p0, x0, -32))

/*
** ld4q_vnum_s8_m36:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_vnum_s8_m36, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, -36),
	   z0 = svld4q_vnum (p0, x0, -36))

/*
** ld4q_vnum_s8_x1:
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
TEST_LOAD (ld4q_vnum_s8_x1, svint8x4_t, int8_t,
	   z0 = svld4q_vnum_s8 (p0, x0, x1),
	   z0 = svld4q_vnum (p0, x0, x1))
