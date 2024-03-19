/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** stnt1_s32_base:
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_base, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0, z0),
		  svstnt1 (pn8, x0, z0))

/*
** stnt1_s32_index:
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, x1, lsl #?2\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_index, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0 + x1, z0),
		  svstnt1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s32_1:
**	incb	x0
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_1, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0 + svcntw (), z0),
		  svstnt1 (pn8, x0 + svcntw (), z0))

/*
** stnt1_s32_2:
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_2, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0 + svcntw () * 2, z0),
		  svstnt1 (pn8, x0 + svcntw () * 2, z0))

/*
** stnt1_s32_14:
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_14, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0 + svcntw () * 14, z0),
		  svstnt1 (pn8, x0 + svcntw () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s32_16:
**	incb	x0, all, mul #16
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_16, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0 + svcntw () * 16, z0),
		  svstnt1 (pn8, x0 + svcntw () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s32_m1:
**	decb	x0
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_m1, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0 - svcntw (), z0),
		  svstnt1 (pn8, x0 - svcntw (), z0))

/*
** stnt1_s32_m2:
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_m2, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0 - svcntw () * 2, z0),
		  svstnt1 (pn8, x0 - svcntw () * 2, z0))

/*
** stnt1_s32_m16:
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_m16, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0 - svcntw () * 16, z0),
		  svstnt1 (pn8, x0 - svcntw () * 16, z0))

/*
** stnt1_s32_m18:
**	addvl	(x[0-9]+), x0, #-18
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[\1\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_m18, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0 - svcntw () * 18, z0),
		  svstnt1 (pn8, x0 - svcntw () * 18, z0))

/*
** stnt1_s32_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	stnt1w	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_z17, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0, z17),
		  svstnt1 (pn8, x0, z17))

/*
** stnt1_s32_z22:
**	stnt1w	{z22\.s(?: - |, )z23\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_z22, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0, z22),
		  svstnt1 (pn8, x0, z22))

/*
** stnt1_s32_z28:
**	stnt1w	{z28\.s(?: - |, )z29\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_z28, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn8, x0, z28),
		  svstnt1 (pn8, x0, z28))

/*
** stnt1_s32_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_pn0, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn0, x0, z0),
		  svstnt1 (pn0, x0, z0))

/*
** stnt1_s32_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_pn7, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn7, x0, z0),
		  svstnt1 (pn7, x0, z0))

/*
** stnt1_s32_pn15:
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s32_pn15, svint32x2_t, int32_t,
		  svstnt1_s32_x2 (pn15, x0, z0),
		  svstnt1 (pn15, x0, z0))

/*
** stnt1_vnum_s32_0:
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s32_0, svint32x2_t, int32_t,
		  svstnt1_vnum_s32_x2 (pn8, x0, 0, z0),
		  svstnt1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s32_1:
**	incb	x0
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s32_1, svint32x2_t, int32_t,
		  svstnt1_vnum_s32_x2 (pn8, x0, 1, z0),
		  svstnt1_vnum (pn8, x0, 1, z0))

/*
** stnt1_vnum_s32_2:
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s32_2, svint32x2_t, int32_t,
		  svstnt1_vnum_s32_x2 (pn8, x0, 2, z0),
		  svstnt1_vnum (pn8, x0, 2, z0))

/*
** stnt1_vnum_s32_14:
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s32_14, svint32x2_t, int32_t,
		  svstnt1_vnum_s32_x2 (pn8, x0, 14, z0),
		  svstnt1_vnum (pn8, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s32_16:
**	incb	x0, all, mul #16
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s32_16, svint32x2_t, int32_t,
		  svstnt1_vnum_s32_x2 (pn8, x0, 16, z0),
		  svstnt1_vnum (pn8, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s32_m1:
**	decb	x0
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s32_m1, svint32x2_t, int32_t,
		  svstnt1_vnum_s32_x2 (pn8, x0, -1, z0),
		  svstnt1_vnum (pn8, x0, -1, z0))

/*
** stnt1_vnum_s32_m2:
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s32_m2, svint32x2_t, int32_t,
		  svstnt1_vnum_s32_x2 (pn8, x0, -2, z0),
		  svstnt1_vnum (pn8, x0, -2, z0))

/*
** stnt1_vnum_s32_m16:
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s32_m16, svint32x2_t, int32_t,
		  svstnt1_vnum_s32_x2 (pn8, x0, -16, z0),
		  svstnt1_vnum (pn8, x0, -16, z0))

/*
** stnt1_vnum_s32_m18:
**	addvl	(x[0-9]+), x0, #-18
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[\1\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s32_m18, svint32x2_t, int32_t,
		  svstnt1_vnum_s32_x2 (pn8, x0, -18, z0),
		  svstnt1_vnum (pn8, x0, -18, z0))

/*
** stnt1_vnum_s32_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	stnt1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, \3\]
** )
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s32_x1, svint32x2_t, int32_t,
		  svstnt1_vnum_s32_x2 (pn8, x0, x1, z0),
		  svstnt1_vnum (pn8, x0, x1, z0))
