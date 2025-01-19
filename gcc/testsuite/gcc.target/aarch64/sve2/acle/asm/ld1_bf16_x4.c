/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** ld1_bf16_base:
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_base, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0),
		 z0 = svld1_x4 (pn8, x0))

/*
** ld1_bf16_index:
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0, x1, lsl #?1\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_index, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0 + x1),
		 z0 = svld1_x4 (pn8, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_bf16_1:
**	incb	x0
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_1, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0 + svcnth ()),
		 z0 = svld1_x4 (pn8, x0 + svcnth ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_bf16_2:
**	incb	x0, all, mul #2
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_2, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0 + svcnth () * 2),
		 z0 = svld1_x4 (pn8, x0 + svcnth () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_bf16_3:
**	incb	x0, all, mul #3
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_3, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0 + svcnth () * 3),
		 z0 = svld1_x4 (pn8, x0 + svcnth () * 3))

/*
** ld1_bf16_4:
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_4, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0 + svcnth () * 4),
		 z0 = svld1_x4 (pn8, x0 + svcnth () * 4))

/*
** ld1_bf16_28:
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_28, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0 + svcnth () * 28),
		 z0 = svld1_x4 (pn8, x0 + svcnth () * 28))

/*
** ld1_bf16_32:
**	[^{]*
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_32, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0 + svcnth () * 32),
		 z0 = svld1_x4 (pn8, x0 + svcnth () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_bf16_m1:
**	decb	x0
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_m1, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0 - svcnth ()),
		 z0 = svld1_x4 (pn8, x0 - svcnth ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_bf16_m2:
**	decb	x0, all, mul #2
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_m2, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0 - svcnth () * 2),
		 z0 = svld1_x4 (pn8, x0 - svcnth () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_bf16_m3:
**	decb	x0, all, mul #3
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_m3, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0 - svcnth () * 3),
		 z0 = svld1_x4 (pn8, x0 - svcnth () * 3))

/*
** ld1_bf16_m4:
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0, #-4, mul vl\]
**	ret
*/
  TEST_LOAD_COUNT (ld1_bf16_m4, svbfloat16x4_t, bfloat16_t,
		   z0 = svld1_bf16_x4 (pn8, x0 - svcnth () * 4),
		   z0 = svld1_x4 (pn8, x0 - svcnth () * 4))

/*
** ld1_bf16_m32:
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_m32, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0 - svcnth () * 32),
		 z0 = svld1_x4 (pn8, x0 - svcnth () * 32))

/*
** ld1_bf16_m36:
**	[^{]*
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_m36, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn8, x0 - svcnth () * 36),
		 z0 = svld1_x4 (pn8, x0 - svcnth () * 36))

/*
** ld1_bf16_z17:
**	ld1h	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_z17, svbfloat16x4_t, bfloat16_t,
		 z17 = svld1_bf16_x4 (pn8, x0),
		 z17 = svld1_x4 (pn8, x0))

/*
** ld1_bf16_z22:
**	ld1h	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_z22, svbfloat16x4_t, bfloat16_t,
		 z22 = svld1_bf16_x4 (pn8, x0),
		 z22 = svld1_x4 (pn8, x0))

/*
** ld1_bf16_z28:
**	ld1h	{z28\.h(?: - |, )z31\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_z28, svbfloat16x4_t, bfloat16_t,
		 z28 = svld1_bf16_x4 (pn8, x0),
		 z28 = svld1_x4 (pn8, x0))

/*
** ld1_bf16_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	ld1h	{z0\.h(?: - |, )z3\.h}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_pn0, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn0, x0),
		 z0 = svld1_x4 (pn0, x0))

/*
** ld1_bf16_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	ld1h	{z0\.h(?: - |, )z3\.h}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_pn7, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn7, x0),
		 z0 = svld1_x4 (pn7, x0))

/*
** ld1_bf16_pn15:
**	ld1h	{z0\.h(?: - |, )z3\.h}, pn15/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_bf16_pn15, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_bf16_x4 (pn15, x0),
		 z0 = svld1_x4 (pn15, x0))

/*
** ld1_vnum_bf16_0:
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_0, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, 0),
		 z0 = svld1_vnum_x4 (pn8, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_bf16_1:
**	incb	x0
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_1, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, 1),
		 z0 = svld1_vnum_x4 (pn8, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_bf16_2:
**	incb	x0, all, mul #2
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_2, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, 2),
		 z0 = svld1_vnum_x4 (pn8, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_bf16_3:
**	incb	x0, all, mul #3
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_3, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, 3),
		 z0 = svld1_vnum_x4 (pn8, x0, 3))

/*
** ld1_vnum_bf16_4:
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_4, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, 4),
		 z0 = svld1_vnum_x4 (pn8, x0, 4))

/*
** ld1_vnum_bf16_28:
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_28, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, 28),
		 z0 = svld1_vnum_x4 (pn8, x0, 28))

/*
** ld1_vnum_bf16_32:
**	[^{]*
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_32, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, 32),
		 z0 = svld1_vnum_x4 (pn8, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_bf16_m1:
**	decb	x0
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_m1, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, -1),
		 z0 = svld1_vnum_x4 (pn8, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_bf16_m2:
**	decb	x0, all, mul #2
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_m2, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, -2),
		 z0 = svld1_vnum_x4 (pn8, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_bf16_m3:
**	decb	x0, all, mul #3
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_m3, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, -3),
		 z0 = svld1_vnum_x4 (pn8, x0, -3))

/*
** ld1_vnum_bf16_m4:
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_m4, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, -4),
		 z0 = svld1_vnum_x4 (pn8, x0, -4))

/*
** ld1_vnum_bf16_m32:
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_m32, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, -32),
		 z0 = svld1_vnum_x4 (pn8, x0, -32))

/*
** ld1_vnum_bf16_m36:
**	[^{]*
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_m36, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, -36),
		 z0 = svld1_vnum_x4 (pn8, x0, -36))

/*
** ld1_vnum_bf16_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ld1h	{z0\.h - z3\.h}, pn8/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_bf16_x1, svbfloat16x4_t, bfloat16_t,
		 z0 = svld1_vnum_bf16_x4 (pn8, x0, x1),
		 z0 = svld1_vnum_x4 (pn8, x0, x1))
