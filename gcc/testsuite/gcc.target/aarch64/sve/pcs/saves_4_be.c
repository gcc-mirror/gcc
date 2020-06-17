/* { dg-do compile } */
/* { dg-options "-O -mbig-endian -fno-stack-clash-protection -g" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

void standard_callee (__SVInt8_t *);

/*
** calls_standard:
**	addvl	sp, sp, #-1
** (
**	stp	x29, x30, \[sp, -16\]!
** |
**	sub	sp, sp, #?16
**	stp	x29, x30, \[sp\]
** )
**	mov	x29, sp
**	addvl	sp, sp, #-18
**	str	p4, \[sp\]
**	str	p5, \[sp, #1, mul vl\]
**	str	p6, \[sp, #2, mul vl\]
**	str	p7, \[sp, #3, mul vl\]
**	str	p8, \[sp, #4, mul vl\]
**	str	p9, \[sp, #5, mul vl\]
**	str	p10, \[sp, #6, mul vl\]
**	str	p11, \[sp, #7, mul vl\]
**	str	p12, \[sp, #8, mul vl\]
**	str	p13, \[sp, #9, mul vl\]
**	str	p14, \[sp, #10, mul vl\]
**	str	p15, \[sp, #11, mul vl\]
**	ptrue	p0\.b, all
**	st1d	z8\.d, p0, \[sp, #2, mul vl\]
**	st1d	z9\.d, p0, \[sp, #3, mul vl\]
**	st1d	z10\.d, p0, \[sp, #4, mul vl\]
**	st1d	z11\.d, p0, \[sp, #5, mul vl\]
**	st1d	z12\.d, p0, \[sp, #6, mul vl\]
**	st1d	z13\.d, p0, \[sp, #7, mul vl\]
**	addvl	x11, sp, #16
**	st1d	z14\.d, p0, \[x11, #-8, mul vl\]
**	st1d	z15\.d, p0, \[x11, #-7, mul vl\]
**	str	z16, \[sp, #10, mul vl\]
**	str	z17, \[sp, #11, mul vl\]
**	str	z18, \[sp, #12, mul vl\]
**	str	z19, \[sp, #13, mul vl\]
**	str	z20, \[sp, #14, mul vl\]
**	str	z21, \[sp, #15, mul vl\]
**	str	z22, \[sp, #16, mul vl\]
**	str	z23, \[sp, #17, mul vl\]
**	addvl	x0, sp, #18
**	add	x0, x0, #?16
**	bl	standard_callee
**	ptrue	p0\.b, all
**	ld1d	z8\.d, p0/z, \[sp, #2, mul vl\]
**	ld1d	z9\.d, p0/z, \[sp, #3, mul vl\]
**	ld1d	z10\.d, p0/z, \[sp, #4, mul vl\]
**	ld1d	z11\.d, p0/z, \[sp, #5, mul vl\]
**	ld1d	z12\.d, p0/z, \[sp, #6, mul vl\]
**	ld1d	z13\.d, p0/z, \[sp, #7, mul vl\]
**	addvl	x11, sp, #16
**	ld1d	z14\.d, p0/z, \[x11, #-8, mul vl\]
**	ld1d	z15\.d, p0/z, \[x11, #-7, mul vl\]
**	ldr	z16, \[sp, #10, mul vl\]
**	ldr	z17, \[sp, #11, mul vl\]
**	ldr	z18, \[sp, #12, mul vl\]
**	ldr	z19, \[sp, #13, mul vl\]
**	ldr	z20, \[sp, #14, mul vl\]
**	ldr	z21, \[sp, #15, mul vl\]
**	ldr	z22, \[sp, #16, mul vl\]
**	ldr	z23, \[sp, #17, mul vl\]
**	ldr	p4, \[sp\]
**	ldr	p5, \[sp, #1, mul vl\]
**	ldr	p6, \[sp, #2, mul vl\]
**	ldr	p7, \[sp, #3, mul vl\]
**	ldr	p8, \[sp, #4, mul vl\]
**	ldr	p9, \[sp, #5, mul vl\]
**	ldr	p10, \[sp, #6, mul vl\]
**	ldr	p11, \[sp, #7, mul vl\]
**	ldr	p12, \[sp, #8, mul vl\]
**	ldr	p13, \[sp, #9, mul vl\]
**	ldr	p14, \[sp, #10, mul vl\]
**	ldr	p15, \[sp, #11, mul vl\]
**	addvl	sp, sp, #18
** (
**	ldp	x29, x30, \[sp\], 16
**	addvl	sp, sp, #1
** |
**	ldp	x29, x30, \[sp\]
**	addvl	sp, sp, #1
**	add	sp, sp, #?16
** )
**	ret
*/
void calls_standard (__SVInt8_t x) { __SVInt8_t tmp; standard_callee (&tmp); }
