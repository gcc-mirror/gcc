/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** ld4q_u64_base:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_u64_base, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0),
	   z0 = svld4q (p0, x0))

/*
** ld4q_u64_index:
**	add	(x[0-9]), x0, x1, lsl #?3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld4q_u64_index, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 + x1),
	   z0 = svld4q (p0, x0 + x1))

/*
** ld4q_u64_index2:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_LOAD (ld4q_u64_index2, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 + x1 * 2),
	   z0 = svld4q (p0, x0 + x1 * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_u64_1:
**	incb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_u64_1, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 + svcntd ()),
	   z0 = svld4q (p0, x0 + svcntd ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_u64_2:
**	incb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_u64_2, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 + svcntd () * 2),
	   z0 = svld4q (p0, x0 + svcntd () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_u64_3:
**	incb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_u64_3, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 + svcntd () * 3),
	   z0 = svld4q (p0, x0 + svcntd () * 3))

/*
** ld4q_u64_4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_u64_4, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 + svcntd () * 4),
	   z0 = svld4q (p0, x0 + svcntd () * 4))

/*
** ld4q_u64_28:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_u64_28, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 + svcntd () * 28),
	   z0 = svld4q (p0, x0 + svcntd () * 28))

/*
** ld4q_u64_32:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_u64_32, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 + svcntd () * 32),
	   z0 = svld4q (p0, x0 + svcntd () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_u64_m1:
**	decb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_u64_m1, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 - svcntd ()),
	   z0 = svld4q (p0, x0 - svcntd ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_u64_m2:
**	decb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_u64_m2, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 - svcntd () * 2),
	   z0 = svld4q (p0, x0 - svcntd () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_u64_m3:
**	decb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_u64_m3, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 - svcntd () * 3),
	   z0 = svld4q (p0, x0 - svcntd () * 3))

/*
** ld4q_u64_m4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_u64_m4, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 - svcntd () * 4),
	   z0 = svld4q (p0, x0 - svcntd () * 4))

/*
** ld4q_u64_m32:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_u64_m32, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 - svcntd () * 32),
	   z0 = svld4q (p0, x0 - svcntd () * 32))

/*
** ld4q_u64_m36:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_u64_m36, svuint64x4_t, uint64_t,
	   z0 = svld4q_u64 (p0, x0 - svcntd () * 36),
	   z0 = svld4q (p0, x0 - svcntd () * 36))

/*
** ld4q_vnum_u64_0:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_0, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, 0),
	   z0 = svld4q_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_u64_1:
**	incb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_1, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, 1),
	   z0 = svld4q_vnum (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_u64_2:
**	incb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_2, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, 2),
	   z0 = svld4q_vnum (p0, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_u64_3:
**	incb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_3, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, 3),
	   z0 = svld4q_vnum (p0, x0, 3))

/*
** ld4q_vnum_u64_4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_4, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, 4),
	   z0 = svld4q_vnum (p0, x0, 4))

/*
** ld4q_vnum_u64_28:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_28, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, 28),
	   z0 = svld4q_vnum (p0, x0, 28))

/*
** ld4q_vnum_u64_32:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_32, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, 32),
	   z0 = svld4q_vnum (p0, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_u64_m1:
**	decb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_m1, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, -1),
	   z0 = svld4q_vnum (p0, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_u64_m2:
**	decb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_m2, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, -2),
	   z0 = svld4q_vnum (p0, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_u64_m3:
**	decb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_m3, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, -3),
	   z0 = svld4q_vnum (p0, x0, -3))

/*
** ld4q_vnum_u64_m4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_m4, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, -4),
	   z0 = svld4q_vnum (p0, x0, -4))

/*
** ld4q_vnum_u64_m32:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_m32, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, -32),
	   z0 = svld4q_vnum (p0, x0, -32))

/*
** ld4q_vnum_u64_m36:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_vnum_u64_m36, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, -36),
	   z0 = svld4q_vnum (p0, x0, -36))

/*
** ld4q_vnum_u64_x1:
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
TEST_LOAD (ld4q_vnum_u64_x1, svuint64x4_t, uint64_t,
	   z0 = svld4q_vnum_u64 (p0, x0, x1),
	   z0 = svld4q_vnum (p0, x0, x1))
