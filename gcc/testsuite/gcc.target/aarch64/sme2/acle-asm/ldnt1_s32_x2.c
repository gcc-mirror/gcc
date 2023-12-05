/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** ldnt1_s32_base:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_base, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn8, x0),
		 z0 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_s32_index:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, x1, lsl #?2\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_index, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn8, x0 + x1),
		 z0 = svldnt1_x2 (pn8, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_s32_1:
**	incb	x0
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_1, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn8, x0 + svcntw ()),
		 z0 = svldnt1_x2 (pn8, x0 + svcntw ()))

/*
** ldnt1_s32_2:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_2, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn8, x0 + svcntw () * 2),
		 z0 = svldnt1_x2 (pn8, x0 + svcntw () * 2))

/*
** ldnt1_s32_14:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_14, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn8, x0 + svcntw () * 14),
		 z0 = svldnt1_x2 (pn8, x0 + svcntw () * 14))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_s32_16:
**	incb	x0, all, mul #16
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_16, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn8, x0 + svcntw () * 16),
		 z0 = svldnt1_x2 (pn8, x0 + svcntw () * 16))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_s32_m1:
**	decb	x0
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_m1, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn8, x0 - svcntw ()),
		 z0 = svldnt1_x2 (pn8, x0 - svcntw ()))

/*
** ldnt1_s32_m2:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_m2, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn8, x0 - svcntw () * 2),
		 z0 = svldnt1_x2 (pn8, x0 - svcntw () * 2))

/*
** ldnt1_s32_m16:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_m16, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn8, x0 - svcntw () * 16),
		 z0 = svldnt1_x2 (pn8, x0 - svcntw () * 16))

/*
** ldnt1_s32_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[\1\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_m18, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn8, x0 - svcntw () * 18),
		 z0 = svldnt1_x2 (pn8, x0 - svcntw () * 18))

/*
** ldnt1_s32_z17:
**	ldnt1w	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_z17, svint32x2_t, int32_t,
		 z17 = svldnt1_s32_x2 (pn8, x0),
		 z17 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_s32_z22:
**	ldnt1w	{z22\.s(?: - |, )z23\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_z22, svint32x2_t, int32_t,
		 z22 = svldnt1_s32_x2 (pn8, x0),
		 z22 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_s32_z28:
**	ldnt1w	{z28\.s(?: - |, )z29\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_z28, svint32x2_t, int32_t,
		 z28 = svldnt1_s32_x2 (pn8, x0),
		 z28 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_s32_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_pn0, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn0, x0),
		 z0 = svldnt1_x2 (pn0, x0))

/*
** ldnt1_s32_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_pn7, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn7, x0),
		 z0 = svldnt1_x2 (pn7, x0))

/*
** ldnt1_s32_pn15:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn15/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s32_pn15, svint32x2_t, int32_t,
		 z0 = svldnt1_s32_x2 (pn15, x0),
		 z0 = svldnt1_x2 (pn15, x0))

/*
** ldnt1_vnum_s32_0:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s32_0, svint32x2_t, int32_t,
		 z0 = svldnt1_vnum_s32_x2 (pn8, x0, 0),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_s32_1:
**	incb	x0
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s32_1, svint32x2_t, int32_t,
		 z0 = svldnt1_vnum_s32_x2 (pn8, x0, 1),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 1))

/*
** ldnt1_vnum_s32_2:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s32_2, svint32x2_t, int32_t,
		 z0 = svldnt1_vnum_s32_x2 (pn8, x0, 2),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 2))

/*
** ldnt1_vnum_s32_14:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s32_14, svint32x2_t, int32_t,
		 z0 = svldnt1_vnum_s32_x2 (pn8, x0, 14),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 14))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_s32_16:
**	incb	x0, all, mul #16
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s32_16, svint32x2_t, int32_t,
		 z0 = svldnt1_vnum_s32_x2 (pn8, x0, 16),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 16))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_s32_m1:
**	decb	x0
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s32_m1, svint32x2_t, int32_t,
		 z0 = svldnt1_vnum_s32_x2 (pn8, x0, -1),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -1))

/*
** ldnt1_vnum_s32_m2:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s32_m2, svint32x2_t, int32_t,
		 z0 = svldnt1_vnum_s32_x2 (pn8, x0, -2),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -2))

/*
** ldnt1_vnum_s32_m16:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s32_m16, svint32x2_t, int32_t,
		 z0 = svldnt1_vnum_s32_x2 (pn8, x0, -16),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -16))

/*
** ldnt1_vnum_s32_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[\1\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s32_m18, svint32x2_t, int32_t,
		 z0 = svldnt1_vnum_s32_x2 (pn8, x0, -18),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -18))

/*
** ldnt1_vnum_s32_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s32_x1, svint32x2_t, int32_t,
		 z0 = svldnt1_vnum_s32_x2 (pn8, x0, x1),
		 z0 = svldnt1_vnum_x2 (pn8, x0, x1))
