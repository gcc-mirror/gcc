/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** st1_bf16_base:
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_base, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0, z0),
		  svst1 (pn8, x0, z0))

/*
** st1_bf16_index:
**	st1h	{z0\.h - z3\.h}, pn8, \[x0, x1, lsl #?1\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_index, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 + x1, z0),
		  svst1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_bf16_1:
**	incb	x0
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_1, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 + svcnth (), z0),
		  svst1 (pn8, x0 + svcnth (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_bf16_2:
**	incb	x0, all, mul #2
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_2, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 + svcnth () * 2, z0),
		  svst1 (pn8, x0 + svcnth () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_bf16_3:
**	incb	x0, all, mul #3
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_3, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 + svcnth () * 3, z0),
		  svst1 (pn8, x0 + svcnth () * 3, z0))

/*
** st1_bf16_4:
**	st1h	{z0\.h - z3\.h}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_4, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 + svcnth () * 4, z0),
		  svst1 (pn8, x0 + svcnth () * 4, z0))

/*
** st1_bf16_28:
**	st1h	{z0\.h - z3\.h}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_28, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 + svcnth () * 28, z0),
		  svst1 (pn8, x0 + svcnth () * 28, z0))

/*
** st1_bf16_32:
**	[^{]*
**	st1h	{z0\.h - z3\.h}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_32, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 + svcnth () * 32, z0),
		  svst1 (pn8, x0 + svcnth () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_bf16_m1:
**	decb	x0
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_m1, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 - svcnth (), z0),
		  svst1 (pn8, x0 - svcnth (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_bf16_m2:
**	decb	x0, all, mul #2
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_m2, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 - svcnth () * 2, z0),
		  svst1 (pn8, x0 - svcnth () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_bf16_m3:
**	decb	x0, all, mul #3
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_m3, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 - svcnth () * 3, z0),
		  svst1 (pn8, x0 - svcnth () * 3, z0))

/*
** st1_bf16_m4:
**	st1h	{z0\.h - z3\.h}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_m4, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 - svcnth () * 4, z0),
		  svst1 (pn8, x0 - svcnth () * 4, z0))

/*
** st1_bf16_m32:
**	st1h	{z0\.h - z3\.h}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_m32, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 - svcnth () * 32, z0),
		  svst1 (pn8, x0 - svcnth () * 32, z0))

/*
** st1_bf16_m36:
**	[^{]*
**	st1h	{z0\.h - z3\.h}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_m36, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0 - svcnth () * 36, z0),
		  svst1 (pn8, x0 - svcnth () * 36, z0))

/*
** st1_bf16_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	st1h	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_z17, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0, z17),
		  svst1 (pn8, x0, z17))

/*
** st1_bf16_z22:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	st1h	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_z22, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0, z22),
		  svst1 (pn8, x0, z22))

/*
** st1_bf16_z28:
**	st1h	{z28\.h - z31\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_z28, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn8, x0, z28),
		  svst1 (pn8, x0, z28))

/*
** st1_bf16_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	st1h	{z0\.h - z3\.h}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_pn0, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn0, x0, z0),
		  svst1 (pn0, x0, z0))

/*
** st1_bf16_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	st1h	{z0\.h - z3\.h}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_pn7, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn7, x0, z0),
		  svst1 (pn7, x0, z0))

/*
** st1_bf16_pn15:
**	st1h	{z0\.h - z3\.h}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_bf16_pn15, svbfloat16x4_t, bfloat16_t,
		  svst1_bf16_x4 (pn15, x0, z0),
		  svst1 (pn15, x0, z0))

/*
** st1_vnum_bf16_0:
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_0, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, 0, z0),
		  svst1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_bf16_1:
**	incb	x0
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_1, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, 1, z0),
		  svst1_vnum (pn8, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_bf16_2:
**	incb	x0, all, mul #2
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_2, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, 2, z0),
		  svst1_vnum (pn8, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_bf16_3:
**	incb	x0, all, mul #3
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_3, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, 3, z0),
		  svst1_vnum (pn8, x0, 3, z0))

/*
** st1_vnum_bf16_4:
**	st1h	{z0\.h - z3\.h}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_4, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, 4, z0),
		  svst1_vnum (pn8, x0, 4, z0))

/*
** st1_vnum_bf16_28:
**	st1h	{z0\.h - z3\.h}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_28, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, 28, z0),
		  svst1_vnum (pn8, x0, 28, z0))

/*
** st1_vnum_bf16_32:
**	[^{]*
**	st1h	{z0\.h - z3\.h}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_32, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, 32, z0),
		  svst1_vnum (pn8, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_bf16_m1:
**	decb	x0
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_m1, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, -1, z0),
		  svst1_vnum (pn8, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_bf16_m2:
**	decb	x0, all, mul #2
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_m2, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, -2, z0),
		  svst1_vnum (pn8, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_bf16_m3:
**	decb	x0, all, mul #3
**	st1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_m3, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, -3, z0),
		  svst1_vnum (pn8, x0, -3, z0))

/*
** st1_vnum_bf16_m4:
**	st1h	{z0\.h - z3\.h}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_m4, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, -4, z0),
		  svst1_vnum (pn8, x0, -4, z0))

/*
** st1_vnum_bf16_m32:
**	st1h	{z0\.h - z3\.h}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_m32, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, -32, z0),
		  svst1_vnum (pn8, x0, -32, z0))

/*
** st1_vnum_bf16_m36:
**	[^{]*
**	st1h	{z0\.h - z3\.h}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_m36, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, -36, z0),
		  svst1_vnum (pn8, x0, -36, z0))

/*
** st1_vnum_bf16_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st1h	{z0\.h - z3\.h}, pn8, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st1h	{z0\.h - z3\.h}, pn8, \[x0, \3\]
** )
**	ret
*/
TEST_STORE_COUNT (st1_vnum_bf16_x1, svbfloat16x4_t, bfloat16_t,
		  svst1_vnum_bf16_x4 (pn8, x0, x1, z0),
		  svst1_vnum (pn8, x0, x1, z0))
