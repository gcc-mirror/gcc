@ libgcc1 routines for ARM cpu.
@ Division and remainder, from Appendix E of the Sparc Version 8
@ Architecture Manual, with fixes from Gordon Irlam.
@ Rewritten for the ARM by Richard Earnshaw (rwe@pegasus.esprit.ec.org)

/* Copyright (C) 1995 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/*
 * Input: dividend and divisor in r0 and r1 respectively.
 *
 * m4 parameters:
 *  NAME	name of function to generate
 *  OP		OP=div => r0 / r1; OP=mod => r0 % r1
 *  S		S=true => signed; S=false => unsigned
 *
 * Algorithm parameters:
 *  N		how many bits per iteration we try to get (4)
 *  WORDSIZE	total number of bits (32)
 *
 * Derived constants:
 *  TOPBITS	number of bits in the top `decade' of a number
 *
 * Important variables:
 *  Q		the partial quotient under development (initially 0)
 *  R		the remainder so far, initially the dividend
 *  ITER	number of main division loop iterations required;
 *		equal to ceil(log2(quotient) / N).  Note that this
 *		is the log base (2^N) of the quotient.
 *  V		the current comparand, initially divisor*2^(ITER*N-1)
 *
 * Cost:
 *  Current estimate for non-large dividend is
 *	ceil(log2(quotient) / N) * (10 + 7N/2) + C
 *  A large dividend is one greater than 2^(31-TOPBITS) and takes a
 *  different path, as the upper bits of the quotient must be developed
 *  one bit at a time.
 */

/*
define(N, `4')dnl
define(WORDSIZE, `32')dnl
define(TOPBITS, eval(WORDSIZE - N*((WORDSIZE-1)/N)))dnl
dnl
define(dividend, `r0')dnl
define(divisor, `r1')dnl
define(Q, `r2')dnl
define(R, `r3')dnl
define(ITER, `ip')dnl
define(V, `lr')dnl
dnl
dnl m4 reminder: ifelse(a,b,c,d) => if a is b, then c, else d
define(T, `r4')dnl
define(SC, `r5')dnl
ifelse(S, `true', `define(SIGN, `r6')')dnl
define(REGLIST, `ifelse(S, `true', `{r4, r5, r6,', `{r4, r5,')')dnl
define(ret, `ldmia	sp!, REGLIST pc}')dnl
dnl
dnl This is the recursive definition for developing quotient digits.
dnl
dnl Parameters:
dnl  $1	the current depth, 1 <= $1 <= N
dnl  $2	the current accumulation of quotient bits
dnl  N	max depth
dnl
dnl We add a new bit to $2 and either recurse or insert the bits in
dnl the quotient.  R, Q, and V are inputs and outputs as defined above;
dnl the condition codes are expected to reflect the input R, and are
dnl modified to reflect the output R.
dnl
define(DEVELOP_QUOTIENT_BITS,
`	@ depth $1, accumulated bits $2
	mov	V, V, lsr #1
	blt	L.$1.eval(2^N+$2+999)
	@ remainder is positive
	subs	R, R, V
	ifelse($1, N,
	`	ifelse(eval(2*$2+1<0), `0',
		`add	Q, Q, `#'eval($2*2+1)',
		`sub	Q, Q, `#'eval(-($2*2+1))')

		b	9f
	', `	DEVELOP_QUOTIENT_BITS(incr($1), `eval(2*$2+1)')')
L.$1.eval(2^N+$2+999):
	@ remainder is negative
	adds	R, R, V
	ifelse($1, N,
	`	ifelse(eval(2*$2-1<0), `0',
		`add	Q, Q, `#'eval($2*2-1)',
		`sub	Q, Q, `#'eval(-($2*2-1))')
		b	9f

	', `	DEVELOP_QUOTIENT_BITS(incr($1), `eval(2*$2-1)')')
	ifelse($1, 1, `9:')')dnl

#include "trap.h"

ip	.req	r12
sp	.req	r13
lr	.req	r14
pc	.req	r15
.text
	.globl NAME
	.align 0
NAME:
	stmdb	sp!, REGLIST lr}
ifelse(S, `true',
`	@ compute sign of result; if neither is negative, no problem
	ifelse(OP, `div', `eor	SIGN, divisor, dividend	@ compute sign',
		`mov	SIGN, dividend')
	cmp	divisor, #0
	rsbmi	divisor, divisor, #0
	beq	Ldiv_zero
	mov	V, divisor
	movs	R, dividend
	rsbmi	R, R, #0	@ make dividend nonnegative
',
`	@ Ready to divide.  Compute size of quotient; scale comparand.
	movs	V, divisor
	mov	R, dividend
	beq	Ldiv_zero
')

	cmp	R, V			@ if divisor exceeds dividend, done
	mov	Q, #0
	bcc	Lgot_result		@ (and algorithm fails otherwise)
	mov	T, `#'(1 << (WORDSIZE - TOPBITS - 1))
	cmp	R, T
	mov	ITER, #0
	bcc	Lnot_really_big

	@ `Here the dividend is >= 2^(31-N) or so.  We must be careful here,
	@ as our usual N-at-a-shot divide step will cause overflow and havoc.
	@ The number of bits in the result here is N*ITER+SC, where SC <= N.
	@ Compute ITER in an unorthodox manner: know we need to shift V into
	@ the top decade: so do not even bother to compare to R.'
		mov	SC, #1
	1:
		cmp	V, T
		bcs	3f
		mov	V, V, lsl `#'N
		add	ITER, ITER, #1
		b	1b

	@ Now compute SC.
	2:	adds	V, V, V
		add	SC, SC, #1
		bcc	Lnot_too_big

		@ We get here if the divisor overflowed while shifting.
		@ This means that R has the high-order bit set.
		@ Restore V and subtract from R.
		mov	T, T, lsl `#'TOPBITS
		mov	V, V, lsr #1
		add	V, T, V
		sub	SC, SC, #1
		b	Ldo_single_div

	Lnot_too_big:
	3:	cmp	V, R
		bcc	2b
@		beq	Ldo_single_div

	/-* NB: these are commented out in the V8-Sparc manual as well *-/
	/-* (I do not understand this) *-/
	@ V > R: went too far: back up 1 step
	@	srl	V, 1, V
	@	dec	SC
	@ do single-bit divide steps
	@
	@ We have to be careful here.  We know that R >= V, so we can do the
	@ first divide step without thinking.  BUT, the others are conditional,
	@ and are only done if R >= 0.  Because both R and V may have the high-
	@ order bit set in the first step, just falling into the regular
	@ division loop will mess up the first time around.
	@ So we unroll slightly...
	Ldo_single_div:
		subs	SC, SC, #1
		blt	Lend_regular_divide
		sub	R, R, V
		mov	Q, #1
		b	Lend_single_divloop
	Lsingle_divloop:
		cmp	R, #0
		mov	Q, Q, lsl #1
		mov	V, V, lsr #1
		@ R >= 0
		subpl	R, R, V
		addpl	Q, Q, #1
		@ R < 0
		addmi	R, R, V
		submi	Q, Q, #1
	Lend_single_divloop:
		subs	SC, SC, #1
		bge	Lsingle_divloop
		b	Lend_regular_divide

1:
	add	ITER, ITER, #1
Lnot_really_big:
	mov	V, V, lsl `#'N
	cmp	V, R
	bls	1b
	@
	@	HOW CAN ITER EVER BE -1 HERE ?????
	@
	cmn	ITER, #1
	beq	Lgot_result

Ldivloop:
	cmp	R, #0	@ set up for initial iteration
	mov	Q, Q, lsl `#'N
	DEVELOP_QUOTIENT_BITS(1, 0)
Lend_regular_divide:
	subs	ITER, ITER, #1
	bge	Ldivloop
	cmp	R, #0
	@ non-restoring fixup here (one instruction only!)
ifelse(OP, `div',
`	sublt	Q, Q, #1
', `	addlt	R, divisor, R
')

Lgot_result:
ifelse(S, `true',
`	@ check to see if answer should be < 0
	cmp	SIGN, #0
	ifelse(OP, `div', `rsbmi Q, Q, #0', `rsbmi R, R, #0')
')
	ifelse(OP, `div', `mov r0, Q', `mov r0, R')
	ret

Ldiv_zero:
	@ Divide by zero trap.  If it returns, return 0 (about as
	@ wrong as possible, but that is what SunOS does...).
	bl	___div0
	mov	r0, #0
	ret
*/

#ifdef L_udivsi3

ip	.req	r12
sp	.req	r13
lr	.req	r14
pc	.req	r15
.text
	.globl ___udivsi3
	.align 0
___udivsi3:
	stmdb	sp!, {r4, r5, lr}
	@ Ready to divide.  Compute size of quotient; scale comparand.
	movs	lr, r1
	mov	r3, r0
	beq	Ldiv_zero


	cmp	r3, lr			@ if r1 exceeds r0, done
	mov	r2, #0
	bcc	Lgot_result		@ (and algorithm fails otherwise)
	mov	r4, #(1 << (32 - 4 - 1))
	cmp	r3, r4
	mov	ip, #0
	bcc	Lnot_really_big

	@ Here the dividend is >= 2^(31-N) or so.  We must be careful here,
	@ as our usual N-at-a-shot divide step will cause overflow and havoc.
	@ The number of bits in the result here is N*ITER+SC, where SC <= N.
	@ Compute ITER in an unorthodox manner: know we need to shift V into
	@ the top decade: so do not even bother to compare to R.
		mov	r5, #1
	1:
		cmp	lr, r4
		bcs	3f
		mov	lr, lr, lsl #4
		add	ip, ip, #1
		b	1b

	@ Now compute r5.
	2:	adds	lr, lr, lr
		add	r5, r5, #1
		bcc	Lnot_too_big

		@ We get here if the r1 overflowed while shifting.
		@ This means that r3 has the high-order bit set.
		@ Restore lr and subtract from r3.
		mov	r4, r4, lsl #4
		mov	lr, lr, lsr #1
		add	lr, r4, lr
		sub	r5, r5, #1
		b	Ldo_single_div

	Lnot_too_big:
	3:	cmp	lr, r3
		bcc	2b
@		beq	Ldo_single_div

	/* NB: these are commented out in the V8-Sparc manual as well */
	/* (I do not understand this) */
	@ lr > r3: went too far: back up 1 step
	@	srl	lr, 1, lr
	@	dec	r5
	@ do single-bit divide steps
	@
	@ We have to be careful here.  We know that r3 >= lr, so we can do the
	@ first divide step without thinking.  BUT, the others are conditional,
	@ and are only done if r3 >= 0.  Because both r3 and lr may have the high-
	@ order bit set in the first step, just falling into the regular
	@ division loop will mess up the first time around.
	@ So we unroll slightly...
	Ldo_single_div:
		subs	r5, r5, #1
		blt	Lend_regular_divide
		sub	r3, r3, lr
		mov	r2, #1
		b	Lend_single_divloop
	Lsingle_divloop:
		cmp	r3, #0
		mov	r2, r2, lsl #1
		mov	lr, lr, lsr #1
		@ r3 >= 0
		subpl	r3, r3, lr
		addpl	r2, r2, #1
		@ r3 < 0
		addmi	r3, r3, lr
		submi	r2, r2, #1
	Lend_single_divloop:
		subs	r5, r5, #1
		bge	Lsingle_divloop
		b	Lend_regular_divide

1:
	add	ip, ip, #1
Lnot_really_big:
	mov	lr, lr, lsl #4
	cmp	lr, r3
	bls	1b
	@
	@	HOW CAN ip EVER BE -1 HERE ?????
	@
	cmn	ip, #1
	beq	Lgot_result

Ldivloop:
	cmp	r3, #0	@ set up for initial iteration
	mov	r2, r2, lsl #4
		@ depth 1, accumulated bits 0
	mov	lr, lr, lsr #1
	blt	L.1.1015
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 2, accumulated bits 1
	mov	lr, lr, lsr #1
	blt	L.2.1016
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 3, accumulated bits 3
	mov	lr, lr, lsr #1
	blt	L.3.1018
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits 7
	mov	lr, lr, lsr #1
	blt	L.4.1022
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #15

		b	9f
	
L.4.1022:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #13
		b	9f

	
	
L.3.1018:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits 5
	mov	lr, lr, lsr #1
	blt	L.4.1020
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #11

		b	9f
	
L.4.1020:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #9
		b	9f

	
	
	
L.2.1016:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 3, accumulated bits 1
	mov	lr, lr, lsr #1
	blt	L.3.1016
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits 3
	mov	lr, lr, lsr #1
	blt	L.4.1018
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #7

		b	9f
	
L.4.1018:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #5
		b	9f

	
	
L.3.1016:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits 1
	mov	lr, lr, lsr #1
	blt	L.4.1016
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #3

		b	9f
	
L.4.1016:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #1
		b	9f

	
	
	
	
L.1.1015:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 2, accumulated bits -1
	mov	lr, lr, lsr #1
	blt	L.2.1014
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 3, accumulated bits -1
	mov	lr, lr, lsr #1
	blt	L.3.1014
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits -1
	mov	lr, lr, lsr #1
	blt	L.4.1014
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #1

		b	9f
	
L.4.1014:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #3
		b	9f

	
	
L.3.1014:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits -3
	mov	lr, lr, lsr #1
	blt	L.4.1012
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #5

		b	9f
	
L.4.1012:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #7
		b	9f

	
	
	
L.2.1014:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 3, accumulated bits -3
	mov	lr, lr, lsr #1
	blt	L.3.1012
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits -5
	mov	lr, lr, lsr #1
	blt	L.4.1010
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #9

		b	9f
	
L.4.1010:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #11
		b	9f

	
	
L.3.1012:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits -7
	mov	lr, lr, lsr #1
	blt	L.4.1008
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #13

		b	9f
	
L.4.1008:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #15
		b	9f

	
	
	
	
	9:
Lend_regular_divide:
	subs	ip, ip, #1
	bge	Ldivloop
	cmp	r3, #0
	@ non-restoring fixup here (one instruction only!)
	sublt	r2, r2, #1


Lgot_result:

	mov r0, r2
	ldmia	sp!, {r4, r5, pc}

Ldiv_zero:
	@ Divide by zero trap.  If it returns, return 0 (about as
	@ wrong as possible, but that is what SunOS does...).
	bl	___div0
	mov	r0, #0
	ldmia	sp!, {r4, r5, pc}

#endif /* L_udivsi3 */

#ifdef L_divsi3

ip	.req	r12
sp	.req	r13
lr	.req	r14
pc	.req	r15
.text
	.globl ___divsi3
	.align 0
___divsi3:
	stmdb	sp!, {r4, r5, r6, lr}
	@ compute sign of result; if neither is negative, no problem
	eor	r6, r1, r0	@ compute sign
	cmp	r1, #0
	rsbmi	r1, r1, #0
	beq	Ldiv_zero
	mov	lr, r1
	movs	r3, r0
	rsbmi	r3, r3, #0	@ make dividend nonnegative


	cmp	r3, lr			@ if r1 exceeds r0, done
	mov	r2, #0
	bcc	Lgot_result		@ (and algorithm fails otherwise)
	mov	r4, #(1 << (32 - 4 - 1))
	cmp	r3, r4
	mov	ip, #0
	bcc	Lnot_really_big

	@ Here the dividend is >= 2^(31-N) or so.  We must be careful here,
	@ as our usual N-at-a-shot divide step will cause overflow and havoc.
	@ The number of bits in the result here is N*ITER+SC, where SC <= N.
	@ Compute ITER in an unorthodox manner: know we need to shift V into
	@ the top decade: so do not even bother to compare to R.
		mov	r5, #1
	1:
		cmp	lr, r4
		bcs	3f
		mov	lr, lr, lsl #4
		add	ip, ip, #1
		b	1b

	@ Now compute r5.
	2:	adds	lr, lr, lr
		add	r5, r5, #1
		bcc	Lnot_too_big

		@ We get here if the r1 overflowed while shifting.
		@ This means that r3 has the high-order bit set.
		@ Restore lr and subtract from r3.
		mov	r4, r4, lsl #4
		mov	lr, lr, lsr #1
		add	lr, r4, lr
		sub	r5, r5, #1
		b	Ldo_single_div

	Lnot_too_big:
	3:	cmp	lr, r3
		bcc	2b
@		beq	Ldo_single_div

	/* NB: these are commented out in the V8-Sparc manual as well */
	/* (I do not understand this) */
	@ lr > r3: went too far: back up 1 step
	@	srl	lr, 1, lr
	@	dec	r5
	@ do single-bit divide steps
	@
	@ We have to be careful here.  We know that r3 >= lr, so we can do the
	@ first divide step without thinking.  BUT, the others are conditional,
	@ and are only done if r3 >= 0.  Because both r3 and lr may have the high-
	@ order bit set in the first step, just falling into the regular
	@ division loop will mess up the first time around.
	@ So we unroll slightly...
	Ldo_single_div:
		subs	r5, r5, #1
		blt	Lend_regular_divide
		sub	r3, r3, lr
		mov	r2, #1
		b	Lend_single_divloop
	Lsingle_divloop:
		cmp	r3, #0
		mov	r2, r2, lsl #1
		mov	lr, lr, lsr #1
		@ r3 >= 0
		subpl	r3, r3, lr
		addpl	r2, r2, #1
		@ r3 < 0
		addmi	r3, r3, lr
		submi	r2, r2, #1
	Lend_single_divloop:
		subs	r5, r5, #1
		bge	Lsingle_divloop
		b	Lend_regular_divide

1:
	add	ip, ip, #1
Lnot_really_big:
	mov	lr, lr, lsl #4
	cmp	lr, r3
	bls	1b
	@
	@	HOW CAN ip EVER BE -1 HERE ?????
	@
	cmn	ip, #1
	beq	Lgot_result

Ldivloop:
	cmp	r3, #0	@ set up for initial iteration
	mov	r2, r2, lsl #4
		@ depth 1, accumulated bits 0
	mov	lr, lr, lsr #1
	blt	L.1.1015
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 2, accumulated bits 1
	mov	lr, lr, lsr #1
	blt	L.2.1016
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 3, accumulated bits 3
	mov	lr, lr, lsr #1
	blt	L.3.1018
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits 7
	mov	lr, lr, lsr #1
	blt	L.4.1022
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #15

		b	9f
	
L.4.1022:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #13
		b	9f

	
	
L.3.1018:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits 5
	mov	lr, lr, lsr #1
	blt	L.4.1020
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #11

		b	9f
	
L.4.1020:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #9
		b	9f

	
	
	
L.2.1016:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 3, accumulated bits 1
	mov	lr, lr, lsr #1
	blt	L.3.1016
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits 3
	mov	lr, lr, lsr #1
	blt	L.4.1018
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #7

		b	9f
	
L.4.1018:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #5
		b	9f

	
	
L.3.1016:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits 1
	mov	lr, lr, lsr #1
	blt	L.4.1016
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #3

		b	9f
	
L.4.1016:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #1
		b	9f

	
	
	
	
L.1.1015:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 2, accumulated bits -1
	mov	lr, lr, lsr #1
	blt	L.2.1014
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 3, accumulated bits -1
	mov	lr, lr, lsr #1
	blt	L.3.1014
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits -1
	mov	lr, lr, lsr #1
	blt	L.4.1014
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #1

		b	9f
	
L.4.1014:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #3
		b	9f

	
	
L.3.1014:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits -3
	mov	lr, lr, lsr #1
	blt	L.4.1012
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #5

		b	9f
	
L.4.1012:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #7
		b	9f

	
	
	
L.2.1014:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 3, accumulated bits -3
	mov	lr, lr, lsr #1
	blt	L.3.1012
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits -5
	mov	lr, lr, lsr #1
	blt	L.4.1010
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #9

		b	9f
	
L.4.1010:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #11
		b	9f

	
	
L.3.1012:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits -7
	mov	lr, lr, lsr #1
	blt	L.4.1008
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #13

		b	9f
	
L.4.1008:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #15
		b	9f

	
	
	
	
	9:
Lend_regular_divide:
	subs	ip, ip, #1
	bge	Ldivloop
	cmp	r3, #0
	@ non-restoring fixup here (one instruction only!)
	sublt	r2, r2, #1


Lgot_result:
	@ check to see if answer should be < 0
	cmp	r6, #0
	rsbmi r2, r2, #0

	mov r0, r2
	ldmia	sp!, {r4, r5, r6, pc}

Ldiv_zero:
	@ Divide by zero trap.  If it returns, return 0 (about as
	@ wrong as possible, but that is what SunOS does...).
	bl	___div0
	mov	r0, #0
	ldmia	sp!, {r4, r5, r6, pc}

#endif /* L_divsi3 */

#ifdef L_umodsi3

ip	.req	r12
sp	.req	r13
lr	.req	r14
pc	.req	r15
.text
	.globl ___umodsi3
	.align 0
___umodsi3:
	stmdb	sp!, {r4, r5, lr}
	@ Ready to divide.  Compute size of quotient; scale comparand.
	movs	lr, r1
	mov	r3, r0
	beq	Ldiv_zero


	cmp	r3, lr			@ if r1 exceeds r0, done
	mov	r2, #0
	bcc	Lgot_result		@ (and algorithm fails otherwise)
	mov	r4, #(1 << (32 - 4 - 1))
	cmp	r3, r4
	mov	ip, #0
	bcc	Lnot_really_big

	@ Here the dividend is >= 2^(31-N) or so.  We must be careful here,
	@ as our usual N-at-a-shot divide step will cause overflow and havoc.
	@ The number of bits in the result here is N*ITER+SC, where SC <= N.
	@ Compute ITER in an unorthodox manner: know we need to shift V into
	@ the top decade: so do not even bother to compare to R.
		mov	r5, #1
	1:
		cmp	lr, r4
		bcs	3f
		mov	lr, lr, lsl #4
		add	ip, ip, #1
		b	1b

	@ Now compute r5.
	2:	adds	lr, lr, lr
		add	r5, r5, #1
		bcc	Lnot_too_big

		@ We get here if the r1 overflowed while shifting.
		@ This means that r3 has the high-order bit set.
		@ Restore lr and subtract from r3.
		mov	r4, r4, lsl #4
		mov	lr, lr, lsr #1
		add	lr, r4, lr
		sub	r5, r5, #1
		b	Ldo_single_div

	Lnot_too_big:
	3:	cmp	lr, r3
		bcc	2b
@		beq	Ldo_single_div

	/* NB: these are commented out in the V8-Sparc manual as well */
	/* (I do not understand this) */
	@ lr > r3: went too far: back up 1 step
	@	srl	lr, 1, lr
	@	dec	r5
	@ do single-bit divide steps
	@
	@ We have to be careful here.  We know that r3 >= lr, so we can do the
	@ first divide step without thinking.  BUT, the others are conditional,
	@ and are only done if r3 >= 0.  Because both r3 and lr may have the high-
	@ order bit set in the first step, just falling into the regular
	@ division loop will mess up the first time around.
	@ So we unroll slightly...
	Ldo_single_div:
		subs	r5, r5, #1
		blt	Lend_regular_divide
		sub	r3, r3, lr
		mov	r2, #1
		b	Lend_single_divloop
	Lsingle_divloop:
		cmp	r3, #0
		mov	r2, r2, lsl #1
		mov	lr, lr, lsr #1
		@ r3 >= 0
		subpl	r3, r3, lr
		addpl	r2, r2, #1
		@ r3 < 0
		addmi	r3, r3, lr
		submi	r2, r2, #1
	Lend_single_divloop:
		subs	r5, r5, #1
		bge	Lsingle_divloop
		b	Lend_regular_divide

1:
	add	ip, ip, #1
Lnot_really_big:
	mov	lr, lr, lsl #4
	cmp	lr, r3
	bls	1b
	@
	@	HOW CAN ip EVER BE -1 HERE ?????
	@
	cmn	ip, #1
	beq	Lgot_result

Ldivloop:
	cmp	r3, #0	@ set up for initial iteration
	mov	r2, r2, lsl #4
		@ depth 1, accumulated bits 0
	mov	lr, lr, lsr #1
	blt	L.1.1015
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 2, accumulated bits 1
	mov	lr, lr, lsr #1
	blt	L.2.1016
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 3, accumulated bits 3
	mov	lr, lr, lsr #1
	blt	L.3.1018
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits 7
	mov	lr, lr, lsr #1
	blt	L.4.1022
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #15

		b	9f
	
L.4.1022:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #13
		b	9f

	
	
L.3.1018:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits 5
	mov	lr, lr, lsr #1
	blt	L.4.1020
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #11

		b	9f
	
L.4.1020:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #9
		b	9f

	
	
	
L.2.1016:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 3, accumulated bits 1
	mov	lr, lr, lsr #1
	blt	L.3.1016
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits 3
	mov	lr, lr, lsr #1
	blt	L.4.1018
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #7

		b	9f
	
L.4.1018:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #5
		b	9f

	
	
L.3.1016:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits 1
	mov	lr, lr, lsr #1
	blt	L.4.1016
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #3

		b	9f
	
L.4.1016:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #1
		b	9f

	
	
	
	
L.1.1015:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 2, accumulated bits -1
	mov	lr, lr, lsr #1
	blt	L.2.1014
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 3, accumulated bits -1
	mov	lr, lr, lsr #1
	blt	L.3.1014
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits -1
	mov	lr, lr, lsr #1
	blt	L.4.1014
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #1

		b	9f
	
L.4.1014:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #3
		b	9f

	
	
L.3.1014:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits -3
	mov	lr, lr, lsr #1
	blt	L.4.1012
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #5

		b	9f
	
L.4.1012:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #7
		b	9f

	
	
	
L.2.1014:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 3, accumulated bits -3
	mov	lr, lr, lsr #1
	blt	L.3.1012
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits -5
	mov	lr, lr, lsr #1
	blt	L.4.1010
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #9

		b	9f
	
L.4.1010:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #11
		b	9f

	
	
L.3.1012:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits -7
	mov	lr, lr, lsr #1
	blt	L.4.1008
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #13

		b	9f
	
L.4.1008:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #15
		b	9f

	
	
	
	
	9:
Lend_regular_divide:
	subs	ip, ip, #1
	bge	Ldivloop
	cmp	r3, #0
	@ non-restoring fixup here (one instruction only!)
	addlt	r3, r1, r3


Lgot_result:

	mov r0, r3
	ldmia	sp!, {r4, r5, pc}

Ldiv_zero:
	@ Divide by zero trap.  If it returns, return 0 (about as
	@ wrong as possible, but that is what SunOS does...).
	bl	___div0
	mov	r0, #0
	ldmia	sp!, {r4, r5, pc}

#endif /* L_umodsi3 */

#ifdef L_modsi3

ip	.req	r12
sp	.req	r13
lr	.req	r14
pc	.req	r15
.text
	.globl ___modsi3
	.align 0
___modsi3:
	stmdb	sp!, {r4, r5, r6, lr}
	@ compute sign of result; if neither is negative, no problem
	mov	r6, r0
	cmp	r1, #0
	rsbmi	r1, r1, #0
	beq	Ldiv_zero
	mov	lr, r1
	movs	r3, r0
	rsbmi	r3, r3, #0	@ make dividend nonnegative


	cmp	r3, lr			@ if r1 exceeds r0, done
	mov	r2, #0
	bcc	Lgot_result		@ (and algorithm fails otherwise)
	mov	r4, #(1 << (32 - 4 - 1))
	cmp	r3, r4
	mov	ip, #0
	bcc	Lnot_really_big

	@ Here the dividend is >= 2^(31-N) or so.  We must be careful here,
	@ as our usual N-at-a-shot divide step will cause overflow and havoc.
	@ The number of bits in the result here is N*ITER+SC, where SC <= N.
	@ Compute ITER in an unorthodox manner: know we need to shift V into
	@ the top decade: so do not even bother to compare to R.
		mov	r5, #1
	1:
		cmp	lr, r4
		bcs	3f
		mov	lr, lr, lsl #4
		add	ip, ip, #1
		b	1b

	@ Now compute r5.
	2:	adds	lr, lr, lr
		add	r5, r5, #1
		bcc	Lnot_too_big

		@ We get here if the r1 overflowed while shifting.
		@ This means that r3 has the high-order bit set.
		@ Restore lr and subtract from r3.
		mov	r4, r4, lsl #4
		mov	lr, lr, lsr #1
		add	lr, r4, lr
		sub	r5, r5, #1
		b	Ldo_single_div

	Lnot_too_big:
	3:	cmp	lr, r3
		bcc	2b
@		beq	Ldo_single_div

	/* NB: these are commented out in the V8-Sparc manual as well */
	/* (I do not understand this) */
	@ lr > r3: went too far: back up 1 step
	@	srl	lr, 1, lr
	@	dec	r5
	@ do single-bit divide steps
	@
	@ We have to be careful here.  We know that r3 >= lr, so we can do the
	@ first divide step without thinking.  BUT, the others are conditional,
	@ and are only done if r3 >= 0.  Because both r3 and lr may have the high-
	@ order bit set in the first step, just falling into the regular
	@ division loop will mess up the first time around.
	@ So we unroll slightly...
	Ldo_single_div:
		subs	r5, r5, #1
		blt	Lend_regular_divide
		sub	r3, r3, lr
		mov	r2, #1
		b	Lend_single_divloop
	Lsingle_divloop:
		cmp	r3, #0
		mov	r2, r2, lsl #1
		mov	lr, lr, lsr #1
		@ r3 >= 0
		subpl	r3, r3, lr
		addpl	r2, r2, #1
		@ r3 < 0
		addmi	r3, r3, lr
		submi	r2, r2, #1
	Lend_single_divloop:
		subs	r5, r5, #1
		bge	Lsingle_divloop
		b	Lend_regular_divide

1:
	add	ip, ip, #1
Lnot_really_big:
	mov	lr, lr, lsl #4
	cmp	lr, r3
	bls	1b
	@
	@	HOW CAN ip EVER BE -1 HERE ?????
	@
	cmn	ip, #1
	beq	Lgot_result

Ldivloop:
	cmp	r3, #0	@ set up for initial iteration
	mov	r2, r2, lsl #4
		@ depth 1, accumulated bits 0
	mov	lr, lr, lsr #1
	blt	L.1.1015
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 2, accumulated bits 1
	mov	lr, lr, lsr #1
	blt	L.2.1016
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 3, accumulated bits 3
	mov	lr, lr, lsr #1
	blt	L.3.1018
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits 7
	mov	lr, lr, lsr #1
	blt	L.4.1022
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #15

		b	9f
	
L.4.1022:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #13
		b	9f

	
	
L.3.1018:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits 5
	mov	lr, lr, lsr #1
	blt	L.4.1020
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #11

		b	9f
	
L.4.1020:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #9
		b	9f

	
	
	
L.2.1016:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 3, accumulated bits 1
	mov	lr, lr, lsr #1
	blt	L.3.1016
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits 3
	mov	lr, lr, lsr #1
	blt	L.4.1018
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #7

		b	9f
	
L.4.1018:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #5
		b	9f

	
	
L.3.1016:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits 1
	mov	lr, lr, lsr #1
	blt	L.4.1016
	@ remainder is positive
	subs	r3, r3, lr
		add	r2, r2, #3

		b	9f
	
L.4.1016:
	@ remainder is negative
	adds	r3, r3, lr
		add	r2, r2, #1
		b	9f

	
	
	
	
L.1.1015:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 2, accumulated bits -1
	mov	lr, lr, lsr #1
	blt	L.2.1014
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 3, accumulated bits -1
	mov	lr, lr, lsr #1
	blt	L.3.1014
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits -1
	mov	lr, lr, lsr #1
	blt	L.4.1014
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #1

		b	9f
	
L.4.1014:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #3
		b	9f

	
	
L.3.1014:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits -3
	mov	lr, lr, lsr #1
	blt	L.4.1012
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #5

		b	9f
	
L.4.1012:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #7
		b	9f

	
	
	
L.2.1014:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 3, accumulated bits -3
	mov	lr, lr, lsr #1
	blt	L.3.1012
	@ remainder is positive
	subs	r3, r3, lr
			@ depth 4, accumulated bits -5
	mov	lr, lr, lsr #1
	blt	L.4.1010
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #9

		b	9f
	
L.4.1010:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #11
		b	9f

	
	
L.3.1012:
	@ remainder is negative
	adds	r3, r3, lr
			@ depth 4, accumulated bits -7
	mov	lr, lr, lsr #1
	blt	L.4.1008
	@ remainder is positive
	subs	r3, r3, lr
		sub	r2, r2, #13

		b	9f
	
L.4.1008:
	@ remainder is negative
	adds	r3, r3, lr
		sub	r2, r2, #15
		b	9f

	
	
	
	
	9:
Lend_regular_divide:
	subs	ip, ip, #1
	bge	Ldivloop
	cmp	r3, #0
	@ non-restoring fixup here (one instruction only!)
	addlt	r3, r1, r3


Lgot_result:
	@ check to see if answer should be < 0
	cmp	r6, #0
	rsbmi r3, r3, #0

	mov r0, r3
	ldmia	sp!, {r4, r5, r6, pc}

Ldiv_zero:
	@ Divide by zero trap.  If it returns, return 0 (about as
	@ wrong as possible, but that is what SunOS does...).
	bl	___div0
	mov	r0, #0
	ldmia	sp!, {r4, r5, r6, pc}

#endif /* L_modsi3 */

#ifdef L_dvmd_tls

	.globl ___div0
	.align 0
___div0:
	mov	pc, lr

#endif /* L_divmodsi_tools */
