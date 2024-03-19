/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** ldnt1_u32_base:
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_base, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0),
		 z0 = svldnt1_x4 (pn8, x0))

/*
** ldnt1_u32_index:
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0, x1, lsl #?2\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_index, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0 + x1),
		 z0 = svldnt1_x4 (pn8, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_u32_1:
**	incb	x0
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_1, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0 + svcntw ()),
		 z0 = svldnt1_x4 (pn8, x0 + svcntw ()))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_u32_2:
**	incb	x0, all, mul #2
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_2, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0 + svcntw () * 2),
		 z0 = svldnt1_x4 (pn8, x0 + svcntw () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_u32_3:
**	incb	x0, all, mul #3
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_3, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0 + svcntw () * 3),
		 z0 = svldnt1_x4 (pn8, x0 + svcntw () * 3))

/*
** ldnt1_u32_4:
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_4, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0 + svcntw () * 4),
		 z0 = svldnt1_x4 (pn8, x0 + svcntw () * 4))

/*
** ldnt1_u32_28:
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_28, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0 + svcntw () * 28),
		 z0 = svldnt1_x4 (pn8, x0 + svcntw () * 28))

/*
** ldnt1_u32_32:
**	[^{]*
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_32, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0 + svcntw () * 32),
		 z0 = svldnt1_x4 (pn8, x0 + svcntw () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_u32_m1:
**	decb	x0
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_m1, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0 - svcntw ()),
		 z0 = svldnt1_x4 (pn8, x0 - svcntw ()))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_u32_m2:
**	decb	x0, all, mul #2
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_m2, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0 - svcntw () * 2),
		 z0 = svldnt1_x4 (pn8, x0 - svcntw () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_u32_m3:
**	decb	x0, all, mul #3
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_m3, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0 - svcntw () * 3),
		 z0 = svldnt1_x4 (pn8, x0 - svcntw () * 3))

/*
** ldnt1_u32_m4:
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0, #-4, mul vl\]
**	ret
*/
  TEST_LOAD_COUNT (ldnt1_u32_m4, svuint32x4_t, uint32_t,
		   z0 = svldnt1_u32_x4 (pn8, x0 - svcntw () * 4),
		   z0 = svldnt1_x4 (pn8, x0 - svcntw () * 4))

/*
** ldnt1_u32_m32:
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_m32, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0 - svcntw () * 32),
		 z0 = svldnt1_x4 (pn8, x0 - svcntw () * 32))

/*
** ldnt1_u32_m36:
**	[^{]*
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_m36, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn8, x0 - svcntw () * 36),
		 z0 = svldnt1_x4 (pn8, x0 - svcntw () * 36))

/*
** ldnt1_u32_z17:
**	ldnt1w	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_z17, svuint32x4_t, uint32_t,
		 z17 = svldnt1_u32_x4 (pn8, x0),
		 z17 = svldnt1_x4 (pn8, x0))

/*
** ldnt1_u32_z22:
**	ldnt1w	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_z22, svuint32x4_t, uint32_t,
		 z22 = svldnt1_u32_x4 (pn8, x0),
		 z22 = svldnt1_x4 (pn8, x0))

/*
** ldnt1_u32_z28:
**	ldnt1w	{z28\.s(?: - |, )z31\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_z28, svuint32x4_t, uint32_t,
		 z28 = svldnt1_u32_x4 (pn8, x0),
		 z28 = svldnt1_x4 (pn8, x0))

/*
** ldnt1_u32_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	ldnt1w	{z0\.s(?: - |, )z3\.s}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_pn0, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn0, x0),
		 z0 = svldnt1_x4 (pn0, x0))

/*
** ldnt1_u32_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	ldnt1w	{z0\.s(?: - |, )z3\.s}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_pn7, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn7, x0),
		 z0 = svldnt1_x4 (pn7, x0))

/*
** ldnt1_u32_pn15:
**	ldnt1w	{z0\.s(?: - |, )z3\.s}, pn15/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_u32_pn15, svuint32x4_t, uint32_t,
		 z0 = svldnt1_u32_x4 (pn15, x0),
		 z0 = svldnt1_x4 (pn15, x0))

/*
** ldnt1_vnum_u32_0:
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_0, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, 0),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_u32_1:
**	incb	x0
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_1, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, 1),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_u32_2:
**	incb	x0, all, mul #2
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_2, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, 2),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_u32_3:
**	incb	x0, all, mul #3
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_3, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, 3),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 3))

/*
** ldnt1_vnum_u32_4:
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_4, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, 4),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 4))

/*
** ldnt1_vnum_u32_28:
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_28, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, 28),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 28))

/*
** ldnt1_vnum_u32_32:
**	[^{]*
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_32, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, 32),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_u32_m1:
**	decb	x0
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_m1, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, -1),
		 z0 = svldnt1_vnum_x4 (pn8, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_u32_m2:
**	decb	x0, all, mul #2
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_m2, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, -2),
		 z0 = svldnt1_vnum_x4 (pn8, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_u32_m3:
**	decb	x0, all, mul #3
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_m3, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, -3),
		 z0 = svldnt1_vnum_x4 (pn8, x0, -3))

/*
** ldnt1_vnum_u32_m4:
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_m4, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, -4),
		 z0 = svldnt1_vnum_x4 (pn8, x0, -4))

/*
** ldnt1_vnum_u32_m32:
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_m32, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, -32),
		 z0 = svldnt1_vnum_x4 (pn8, x0, -32))

/*
** ldnt1_vnum_u32_m36:
**	[^{]*
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_m36, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, -36),
		 z0 = svldnt1_vnum_x4 (pn8, x0, -36))

/*
** ldnt1_vnum_u32_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ldnt1w	{z0\.s - z3\.s}, pn8/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_u32_x1, svuint32x4_t, uint32_t,
		 z0 = svldnt1_vnum_u32_x4 (pn8, x0, x1),
		 z0 = svldnt1_vnum_x4 (pn8, x0, x1))
