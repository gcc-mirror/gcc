/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** stnt1_mf8_base:
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_base, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0, z0),
		  svstnt1 (pn8, x0, z0))

/*
** stnt1_mf8_index:
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0, x1\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_index, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0 + x1, z0),
		  svstnt1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_mf8_1:
**	incb	x0
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_1, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0 + svcntb (), z0),
		  svstnt1 (pn8, x0 + svcntb (), z0))

/*
** stnt1_mf8_2:
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_2, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0 + svcntb () * 2, z0),
		  svstnt1 (pn8, x0 + svcntb () * 2, z0))

/*
** stnt1_mf8_14:
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_14, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0 + svcntb () * 14, z0),
		  svstnt1 (pn8, x0 + svcntb () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_mf8_16:
**	incb	x0, all, mul #16
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_16, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0 + svcntb () * 16, z0),
		  svstnt1 (pn8, x0 + svcntb () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_mf8_m1:
**	decb	x0
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_m1, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0 - svcntb (), z0),
		  svstnt1 (pn8, x0 - svcntb (), z0))

/*
** stnt1_mf8_m2:
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_m2, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0 - svcntb () * 2, z0),
		  svstnt1 (pn8, x0 - svcntb () * 2, z0))

/*
** stnt1_mf8_m16:
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_m16, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0 - svcntb () * 16, z0),
		  svstnt1 (pn8, x0 - svcntb () * 16, z0))

/*
** stnt1_mf8_m18:
**	addvl	(x[0-9]+), x0, #-18
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[\1\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_m18, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0 - svcntb () * 18, z0),
		  svstnt1 (pn8, x0 - svcntb () * 18, z0))

/*
** stnt1_mf8_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	stnt1b	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_z17, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0, z17),
		  svstnt1 (pn8, x0, z17))

/*
** stnt1_mf8_z22:
**	stnt1b	{z22\.b(?: - |, )z23\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_z22, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0, z22),
		  svstnt1 (pn8, x0, z22))

/*
** stnt1_mf8_z28:
**	stnt1b	{z28\.b(?: - |, )z29\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_z28, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn8, x0, z28),
		  svstnt1 (pn8, x0, z28))

/*
** stnt1_mf8_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_pn0, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn0, x0, z0),
		  svstnt1 (pn0, x0, z0))

/*
** stnt1_mf8_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_pn7, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn7, x0, z0),
		  svstnt1 (pn7, x0, z0))

/*
** stnt1_mf8_pn15:
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_mf8_pn15, svmfloat8x2_t, mfloat8_t,
		  svstnt1_mf8_x2 (pn15, x0, z0),
		  svstnt1 (pn15, x0, z0))

/*
** stnt1_vnum_mf8_0:
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_mf8_0, svmfloat8x2_t, mfloat8_t,
		  svstnt1_vnum_mf8_x2 (pn8, x0, 0, z0),
		  svstnt1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_mf8_1:
**	incb	x0
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_mf8_1, svmfloat8x2_t, mfloat8_t,
		  svstnt1_vnum_mf8_x2 (pn8, x0, 1, z0),
		  svstnt1_vnum (pn8, x0, 1, z0))

/*
** stnt1_vnum_mf8_2:
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_mf8_2, svmfloat8x2_t, mfloat8_t,
		  svstnt1_vnum_mf8_x2 (pn8, x0, 2, z0),
		  svstnt1_vnum (pn8, x0, 2, z0))

/*
** stnt1_vnum_mf8_14:
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_mf8_14, svmfloat8x2_t, mfloat8_t,
		  svstnt1_vnum_mf8_x2 (pn8, x0, 14, z0),
		  svstnt1_vnum (pn8, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_mf8_16:
**	incb	x0, all, mul #16
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_mf8_16, svmfloat8x2_t, mfloat8_t,
		  svstnt1_vnum_mf8_x2 (pn8, x0, 16, z0),
		  svstnt1_vnum (pn8, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_mf8_m1:
**	decb	x0
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_mf8_m1, svmfloat8x2_t, mfloat8_t,
		  svstnt1_vnum_mf8_x2 (pn8, x0, -1, z0),
		  svstnt1_vnum (pn8, x0, -1, z0))

/*
** stnt1_vnum_mf8_m2:
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_mf8_m2, svmfloat8x2_t, mfloat8_t,
		  svstnt1_vnum_mf8_x2 (pn8, x0, -2, z0),
		  svstnt1_vnum (pn8, x0, -2, z0))

/*
** stnt1_vnum_mf8_m16:
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_mf8_m16, svmfloat8x2_t, mfloat8_t,
		  svstnt1_vnum_mf8_x2 (pn8, x0, -16, z0),
		  svstnt1_vnum (pn8, x0, -16, z0))

/*
** stnt1_vnum_mf8_m18:
**	addvl	(x[0-9]+), x0, #-18
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[\1\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_mf8_m18, svmfloat8x2_t, mfloat8_t,
		  svstnt1_vnum_mf8_x2 (pn8, x0, -18, z0),
		  svstnt1_vnum (pn8, x0, -18, z0))

/*
** stnt1_vnum_mf8_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	stnt1b	{z0\.b(?: - |, )z1\.b}, pn8, \[x0, \3\]
** )
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_mf8_x1, svmfloat8x2_t, mfloat8_t,
		  svstnt1_vnum_mf8_x2 (pn8, x0, x1, z0),
		  svstnt1_vnum (pn8, x0, x1, z0))
