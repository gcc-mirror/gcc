/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** ld1_s32_base:
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_base, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0),
		 z0 = svld1_x4 (pn8, x0))

/*
** ld1_s32_index:
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0, x1, lsl #?2\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_index, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0 + x1),
		 z0 = svld1_x4 (pn8, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_s32_1:
**	incb	x0
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_1, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0 + svcntw ()),
		 z0 = svld1_x4 (pn8, x0 + svcntw ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_s32_2:
**	incb	x0, all, mul #2
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_2, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0 + svcntw () * 2),
		 z0 = svld1_x4 (pn8, x0 + svcntw () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_s32_3:
**	incb	x0, all, mul #3
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_3, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0 + svcntw () * 3),
		 z0 = svld1_x4 (pn8, x0 + svcntw () * 3))

/*
** ld1_s32_4:
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_4, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0 + svcntw () * 4),
		 z0 = svld1_x4 (pn8, x0 + svcntw () * 4))

/*
** ld1_s32_28:
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_28, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0 + svcntw () * 28),
		 z0 = svld1_x4 (pn8, x0 + svcntw () * 28))

/*
** ld1_s32_32:
**	[^{]*
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_32, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0 + svcntw () * 32),
		 z0 = svld1_x4 (pn8, x0 + svcntw () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_s32_m1:
**	decb	x0
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_m1, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0 - svcntw ()),
		 z0 = svld1_x4 (pn8, x0 - svcntw ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_s32_m2:
**	decb	x0, all, mul #2
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_m2, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0 - svcntw () * 2),
		 z0 = svld1_x4 (pn8, x0 - svcntw () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_s32_m3:
**	decb	x0, all, mul #3
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_m3, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0 - svcntw () * 3),
		 z0 = svld1_x4 (pn8, x0 - svcntw () * 3))

/*
** ld1_s32_m4:
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0, #-4, mul vl\]
**	ret
*/
  TEST_LOAD_COUNT (ld1_s32_m4, svint32x4_t, int32_t,
		   z0 = svld1_s32_x4 (pn8, x0 - svcntw () * 4),
		   z0 = svld1_x4 (pn8, x0 - svcntw () * 4))

/*
** ld1_s32_m32:
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_m32, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0 - svcntw () * 32),
		 z0 = svld1_x4 (pn8, x0 - svcntw () * 32))

/*
** ld1_s32_m36:
**	[^{]*
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_m36, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn8, x0 - svcntw () * 36),
		 z0 = svld1_x4 (pn8, x0 - svcntw () * 36))

/*
** ld1_s32_z17:
**	ld1w	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_z17, svint32x4_t, int32_t,
		 z17 = svld1_s32_x4 (pn8, x0),
		 z17 = svld1_x4 (pn8, x0))

/*
** ld1_s32_z22:
**	ld1w	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_z22, svint32x4_t, int32_t,
		 z22 = svld1_s32_x4 (pn8, x0),
		 z22 = svld1_x4 (pn8, x0))

/*
** ld1_s32_z28:
**	ld1w	{z28\.s(?: - |, )z31\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_z28, svint32x4_t, int32_t,
		 z28 = svld1_s32_x4 (pn8, x0),
		 z28 = svld1_x4 (pn8, x0))

/*
** ld1_s32_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	ld1w	{z0\.s(?: - |, )z3\.s}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_pn0, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn0, x0),
		 z0 = svld1_x4 (pn0, x0))

/*
** ld1_s32_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	ld1w	{z0\.s(?: - |, )z3\.s}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_pn7, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn7, x0),
		 z0 = svld1_x4 (pn7, x0))

/*
** ld1_s32_pn15:
**	ld1w	{z0\.s(?: - |, )z3\.s}, pn15/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_pn15, svint32x4_t, int32_t,
		 z0 = svld1_s32_x4 (pn15, x0),
		 z0 = svld1_x4 (pn15, x0))

/*
** ld1_vnum_s32_0:
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_0, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, 0),
		 z0 = svld1_vnum_x4 (pn8, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_s32_1:
**	incb	x0
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_1, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, 1),
		 z0 = svld1_vnum_x4 (pn8, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_s32_2:
**	incb	x0, all, mul #2
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_2, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, 2),
		 z0 = svld1_vnum_x4 (pn8, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_s32_3:
**	incb	x0, all, mul #3
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_3, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, 3),
		 z0 = svld1_vnum_x4 (pn8, x0, 3))

/*
** ld1_vnum_s32_4:
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_4, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, 4),
		 z0 = svld1_vnum_x4 (pn8, x0, 4))

/*
** ld1_vnum_s32_28:
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_28, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, 28),
		 z0 = svld1_vnum_x4 (pn8, x0, 28))

/*
** ld1_vnum_s32_32:
**	[^{]*
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_32, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, 32),
		 z0 = svld1_vnum_x4 (pn8, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_s32_m1:
**	decb	x0
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_m1, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, -1),
		 z0 = svld1_vnum_x4 (pn8, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_s32_m2:
**	decb	x0, all, mul #2
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_m2, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, -2),
		 z0 = svld1_vnum_x4 (pn8, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_s32_m3:
**	decb	x0, all, mul #3
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_m3, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, -3),
		 z0 = svld1_vnum_x4 (pn8, x0, -3))

/*
** ld1_vnum_s32_m4:
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_m4, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, -4),
		 z0 = svld1_vnum_x4 (pn8, x0, -4))

/*
** ld1_vnum_s32_m32:
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_m32, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, -32),
		 z0 = svld1_vnum_x4 (pn8, x0, -32))

/*
** ld1_vnum_s32_m36:
**	[^{]*
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_m36, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, -36),
		 z0 = svld1_vnum_x4 (pn8, x0, -36))

/*
** ld1_vnum_s32_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ld1w	{z0\.s - z3\.s}, pn8/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_x1, svint32x4_t, int32_t,
		 z0 = svld1_vnum_s32_x4 (pn8, x0, x1),
		 z0 = svld1_vnum_x4 (pn8, x0, x1))
