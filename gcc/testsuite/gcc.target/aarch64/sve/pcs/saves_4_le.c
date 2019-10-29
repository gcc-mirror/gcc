/* { dg-do compile } */
/* { dg-options "-O -mlittle-endian -fno-stack-clash-protection -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

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
**	addvl	sp, sp, #-17
**	str	p4, \[sp\]
**	str	p5, \[sp, #1, mul vl\]
**	str	p6, \[sp, #2, mul vl\]
**	str	p7, \[sp, #3, mul vl\]
**	str	p8, \[sp, #4, mul vl\]
**	str	p9, \[sp, #5, mul vl\]
**	str	p10, \[sp, #6, mul vl\]
**	str	p11, \[sp, #7, mul vl\]
**	str	z8, \[sp, #1, mul vl\]
**	str	z9, \[sp, #2, mul vl\]
**	str	z10, \[sp, #3, mul vl\]
**	str	z11, \[sp, #4, mul vl\]
**	str	z12, \[sp, #5, mul vl\]
**	str	z13, \[sp, #6, mul vl\]
**	str	z14, \[sp, #7, mul vl\]
**	str	z15, \[sp, #8, mul vl\]
**	str	z16, \[sp, #9, mul vl\]
**	str	z17, \[sp, #10, mul vl\]
**	str	z18, \[sp, #11, mul vl\]
**	str	z19, \[sp, #12, mul vl\]
**	str	z20, \[sp, #13, mul vl\]
**	str	z21, \[sp, #14, mul vl\]
**	str	z22, \[sp, #15, mul vl\]
**	str	z23, \[sp, #16, mul vl\]
**	addvl	x0, sp, #17
**	add	x0, x0, #?16
**	bl	standard_callee
**	ldr	z8, \[sp, #1, mul vl\]
**	ldr	z9, \[sp, #2, mul vl\]
**	ldr	z10, \[sp, #3, mul vl\]
**	ldr	z11, \[sp, #4, mul vl\]
**	ldr	z12, \[sp, #5, mul vl\]
**	ldr	z13, \[sp, #6, mul vl\]
**	ldr	z14, \[sp, #7, mul vl\]
**	ldr	z15, \[sp, #8, mul vl\]
**	ldr	z16, \[sp, #9, mul vl\]
**	ldr	z17, \[sp, #10, mul vl\]
**	ldr	z18, \[sp, #11, mul vl\]
**	ldr	z19, \[sp, #12, mul vl\]
**	ldr	z20, \[sp, #13, mul vl\]
**	ldr	z21, \[sp, #14, mul vl\]
**	ldr	z22, \[sp, #15, mul vl\]
**	ldr	z23, \[sp, #16, mul vl\]
**	ldr	p4, \[sp\]
**	ldr	p5, \[sp, #1, mul vl\]
**	ldr	p6, \[sp, #2, mul vl\]
**	ldr	p7, \[sp, #3, mul vl\]
**	ldr	p8, \[sp, #4, mul vl\]
**	ldr	p9, \[sp, #5, mul vl\]
**	ldr	p10, \[sp, #6, mul vl\]
**	ldr	p11, \[sp, #7, mul vl\]
**	addvl	sp, sp, #17
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
