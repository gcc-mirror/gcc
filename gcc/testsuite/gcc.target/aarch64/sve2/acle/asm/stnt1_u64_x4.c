/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** stnt1_u64_base:
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_base, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0, z0),
		  svstnt1 (pn8, x0, z0))

/*
** stnt1_u64_index:
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0, x1, lsl #?3\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_index, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 + x1, z0),
		  svstnt1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_u64_1:
**	incb	x0
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_1, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 + svcntd (), z0),
		  svstnt1 (pn8, x0 + svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_u64_2:
**	incb	x0, all, mul #2
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_2, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 + svcntd () * 2, z0),
		  svstnt1 (pn8, x0 + svcntd () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_u64_3:
**	incb	x0, all, mul #3
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_3, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 + svcntd () * 3, z0),
		  svstnt1 (pn8, x0 + svcntd () * 3, z0))

/*
** stnt1_u64_4:
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_4, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 + svcntd () * 4, z0),
		  svstnt1 (pn8, x0 + svcntd () * 4, z0))

/*
** stnt1_u64_28:
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_28, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 + svcntd () * 28, z0),
		  svstnt1 (pn8, x0 + svcntd () * 28, z0))

/*
** stnt1_u64_32:
**	[^{]*
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_32, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 + svcntd () * 32, z0),
		  svstnt1 (pn8, x0 + svcntd () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_u64_m1:
**	decb	x0
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_m1, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 - svcntd (), z0),
		  svstnt1 (pn8, x0 - svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_u64_m2:
**	decb	x0, all, mul #2
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_m2, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 - svcntd () * 2, z0),
		  svstnt1 (pn8, x0 - svcntd () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_u64_m3:
**	decb	x0, all, mul #3
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_m3, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 - svcntd () * 3, z0),
		  svstnt1 (pn8, x0 - svcntd () * 3, z0))

/*
** stnt1_u64_m4:
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_m4, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 - svcntd () * 4, z0),
		  svstnt1 (pn8, x0 - svcntd () * 4, z0))

/*
** stnt1_u64_m32:
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_m32, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 - svcntd () * 32, z0),
		  svstnt1 (pn8, x0 - svcntd () * 32, z0))

/*
** stnt1_u64_m36:
**	[^{]*
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_m36, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0 - svcntd () * 36, z0),
		  svstnt1 (pn8, x0 - svcntd () * 36, z0))

/*
** stnt1_u64_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	stnt1d	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_z17, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0, z17),
		  svstnt1 (pn8, x0, z17))

/*
** stnt1_u64_z22:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	stnt1d	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_z22, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0, z22),
		  svstnt1 (pn8, x0, z22))

/*
** stnt1_u64_z28:
**	stnt1d	{z28\.d - z31\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_z28, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn8, x0, z28),
		  svstnt1 (pn8, x0, z28))

/*
** stnt1_u64_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	stnt1d	{z0\.d - z3\.d}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_pn0, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn0, x0, z0),
		  svstnt1 (pn0, x0, z0))

/*
** stnt1_u64_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	stnt1d	{z0\.d - z3\.d}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_pn7, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn7, x0, z0),
		  svstnt1 (pn7, x0, z0))

/*
** stnt1_u64_pn15:
**	stnt1d	{z0\.d - z3\.d}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_u64_pn15, svuint64x4_t, uint64_t,
		  svstnt1_u64_x4 (pn15, x0, z0),
		  svstnt1 (pn15, x0, z0))

/*
** stnt1_vnum_u64_0:
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_0, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, 0, z0),
		  svstnt1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_u64_1:
**	incb	x0
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_1, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, 1, z0),
		  svstnt1_vnum (pn8, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_u64_2:
**	incb	x0, all, mul #2
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_2, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, 2, z0),
		  svstnt1_vnum (pn8, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_u64_3:
**	incb	x0, all, mul #3
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_3, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, 3, z0),
		  svstnt1_vnum (pn8, x0, 3, z0))

/*
** stnt1_vnum_u64_4:
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_4, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, 4, z0),
		  svstnt1_vnum (pn8, x0, 4, z0))

/*
** stnt1_vnum_u64_28:
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_28, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, 28, z0),
		  svstnt1_vnum (pn8, x0, 28, z0))

/*
** stnt1_vnum_u64_32:
**	[^{]*
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_32, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, 32, z0),
		  svstnt1_vnum (pn8, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_u64_m1:
**	decb	x0
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_m1, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, -1, z0),
		  svstnt1_vnum (pn8, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_u64_m2:
**	decb	x0, all, mul #2
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_m2, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, -2, z0),
		  svstnt1_vnum (pn8, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_u64_m3:
**	decb	x0, all, mul #3
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_m3, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, -3, z0),
		  svstnt1_vnum (pn8, x0, -3, z0))

/*
** stnt1_vnum_u64_m4:
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_m4, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, -4, z0),
		  svstnt1_vnum (pn8, x0, -4, z0))

/*
** stnt1_vnum_u64_m32:
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_m32, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, -32, z0),
		  svstnt1_vnum (pn8, x0, -32, z0))

/*
** stnt1_vnum_u64_m36:
**	[^{]*
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_m36, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, -36, z0),
		  svstnt1_vnum (pn8, x0, -36, z0))

/*
** stnt1_vnum_u64_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	stnt1d	{z0\.d - z3\.d}, pn8, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	stnt1d	{z0\.d - z3\.d}, pn8, \[x0, \3\]
** )
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_u64_x1, svuint64x4_t, uint64_t,
		  svstnt1_vnum_u64_x4 (pn8, x0, x1, z0),
		  svstnt1_vnum (pn8, x0, x1, z0))
