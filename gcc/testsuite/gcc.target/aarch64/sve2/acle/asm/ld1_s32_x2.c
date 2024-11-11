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
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_base, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn8, x0),
		 z0 = svld1_x2 (pn8, x0))

/*
** ld1_s32_index:
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, x1, lsl #?2\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_index, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn8, x0 + x1),
		 z0 = svld1_x2 (pn8, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_s32_1:
**	incb	x0
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_1, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn8, x0 + svcntw ()),
		 z0 = svld1_x2 (pn8, x0 + svcntw ()))

/*
** ld1_s32_2:
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_2, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn8, x0 + svcntw () * 2),
		 z0 = svld1_x2 (pn8, x0 + svcntw () * 2))

/*
** ld1_s32_14:
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_14, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn8, x0 + svcntw () * 14),
		 z0 = svld1_x2 (pn8, x0 + svcntw () * 14))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_s32_16:
**	incb	x0, all, mul #16
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_16, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn8, x0 + svcntw () * 16),
		 z0 = svld1_x2 (pn8, x0 + svcntw () * 16))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_s32_m1:
**	decb	x0
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_m1, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn8, x0 - svcntw ()),
		 z0 = svld1_x2 (pn8, x0 - svcntw ()))

/*
** ld1_s32_m2:
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_m2, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn8, x0 - svcntw () * 2),
		 z0 = svld1_x2 (pn8, x0 - svcntw () * 2))

/*
** ld1_s32_m16:
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_m16, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn8, x0 - svcntw () * 16),
		 z0 = svld1_x2 (pn8, x0 - svcntw () * 16))

/*
** ld1_s32_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[\1\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_m18, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn8, x0 - svcntw () * 18),
		 z0 = svld1_x2 (pn8, x0 - svcntw () * 18))

/*
** ld1_s32_z17:
**	ld1w	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_z17, svint32x2_t, int32_t,
		 z17 = svld1_s32_x2 (pn8, x0),
		 z17 = svld1_x2 (pn8, x0))

/*
** ld1_s32_z22:
**	ld1w	{z22\.s(?: - |, )z23\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_z22, svint32x2_t, int32_t,
		 z22 = svld1_s32_x2 (pn8, x0),
		 z22 = svld1_x2 (pn8, x0))

/*
** ld1_s32_z28:
**	ld1w	{z28\.s(?: - |, )z29\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_z28, svint32x2_t, int32_t,
		 z28 = svld1_s32_x2 (pn8, x0),
		 z28 = svld1_x2 (pn8, x0))

/*
** ld1_s32_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_pn0, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn0, x0),
		 z0 = svld1_x2 (pn0, x0))

/*
** ld1_s32_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_pn7, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn7, x0),
		 z0 = svld1_x2 (pn7, x0))

/*
** ld1_s32_pn15:
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn15/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_s32_pn15, svint32x2_t, int32_t,
		 z0 = svld1_s32_x2 (pn15, x0),
		 z0 = svld1_x2 (pn15, x0))

/*
** ld1_vnum_s32_0:
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_0, svint32x2_t, int32_t,
		 z0 = svld1_vnum_s32_x2 (pn8, x0, 0),
		 z0 = svld1_vnum_x2 (pn8, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_s32_1:
**	incb	x0
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_1, svint32x2_t, int32_t,
		 z0 = svld1_vnum_s32_x2 (pn8, x0, 1),
		 z0 = svld1_vnum_x2 (pn8, x0, 1))

/*
** ld1_vnum_s32_2:
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_2, svint32x2_t, int32_t,
		 z0 = svld1_vnum_s32_x2 (pn8, x0, 2),
		 z0 = svld1_vnum_x2 (pn8, x0, 2))

/*
** ld1_vnum_s32_14:
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_14, svint32x2_t, int32_t,
		 z0 = svld1_vnum_s32_x2 (pn8, x0, 14),
		 z0 = svld1_vnum_x2 (pn8, x0, 14))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_s32_16:
**	incb	x0, all, mul #16
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_16, svint32x2_t, int32_t,
		 z0 = svld1_vnum_s32_x2 (pn8, x0, 16),
		 z0 = svld1_vnum_x2 (pn8, x0, 16))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_s32_m1:
**	decb	x0
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_m1, svint32x2_t, int32_t,
		 z0 = svld1_vnum_s32_x2 (pn8, x0, -1),
		 z0 = svld1_vnum_x2 (pn8, x0, -1))

/*
** ld1_vnum_s32_m2:
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_m2, svint32x2_t, int32_t,
		 z0 = svld1_vnum_s32_x2 (pn8, x0, -2),
		 z0 = svld1_vnum_x2 (pn8, x0, -2))

/*
** ld1_vnum_s32_m16:
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_m16, svint32x2_t, int32_t,
		 z0 = svld1_vnum_s32_x2 (pn8, x0, -16),
		 z0 = svld1_vnum_x2 (pn8, x0, -16))

/*
** ld1_vnum_s32_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[\1\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_m18, svint32x2_t, int32_t,
		 z0 = svld1_vnum_s32_x2 (pn8, x0, -18),
		 z0 = svld1_vnum_x2 (pn8, x0, -18))

/*
** ld1_vnum_s32_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ld1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_s32_x1, svint32x2_t, int32_t,
		 z0 = svld1_vnum_s32_x2 (pn8, x0, x1),
		 z0 = svld1_vnum_x2 (pn8, x0, x1))
