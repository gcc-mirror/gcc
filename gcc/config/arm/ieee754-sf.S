/* ieee754-sf.S single-precision floating point support for ARM

   Copyright (C) 2003, 2004  Free Software Foundation, Inc.
   Contributed by Nicolas Pitre (nico@cam.org)

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   In addition to the permissions in the GNU General Public License, the
   Free Software Foundation gives you unlimited permission to link the
   compiled version of this file into combinations with other programs,
   and to distribute those combinations without any restriction coming
   from the use of this file.  (The General Public License restrictions
   do apply in other respects; for example, they cover modification of
   the file, and distribution when not linked into a combine
   executable.)

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/*
 * Notes:
 *
 * The goal of this code is to be as fast as possible.  This is
 * not meant to be easy to understand for the casual reader.
 *
 * Only the default rounding mode is intended for best performances.
 * Exceptions aren't supported yet, but that can be added quite easily
 * if necessary without impacting performances.
 */

#ifdef L_negsf2
	
ARM_FUNC_START negsf2
	eor	r0, r0, #0x80000000	@ flip sign bit
	RET

	FUNC_END negsf2

#endif

#ifdef L_addsubsf3

ARM_FUNC_START subsf3
	eor	r1, r1, #0x80000000	@ flip sign bit of second arg
#if defined(__thumb__) && !defined(__THUMB_INTERWORK__)
	b	1f			@ Skip Thumb-code prologue
#endif

ARM_FUNC_START addsf3

1:	@ Compare both args, return zero if equal but the sign.
	eor	r2, r0, r1
	teq	r2, #0x80000000
	beq	LSYM(Lad_z)

	@ If first arg is 0 or -0, return second arg.
	@ If second arg is 0 or -0, return first arg.
	bics	r2, r0, #0x80000000
	moveq	r0, r1
	bicnes	r2, r1, #0x80000000
	RETc(eq)

	@ Mask out exponents.
	mov	ip, #0xff000000
	and	r2, r0, ip, lsr #1
	and	r3, r1, ip, lsr #1

	@ If either of them is 255, result will be INF or NAN
	teq	r2, ip, lsr #1
	teqne	r3, ip, lsr #1
	beq	LSYM(Lad_i)

	@ Compute exponent difference.  Make largest exponent in r2,
	@ corresponding arg in r0, and positive exponent difference in r3.
	subs	r3, r3, r2
	addgt	r2, r2, r3
	eorgt	r1, r0, r1
	eorgt	r0, r1, r0
	eorgt	r1, r0, r1
	rsblt	r3, r3, #0

	@ If exponent difference is too large, return largest argument
	@ already in r0.  We need up to 25 bit to handle proper rounding
	@ of 0x1p25 - 1.1.
	cmp	r3, #(25 << 23)
	RETc(hi)

	@ Convert mantissa to signed integer.
	tst	r0, #0x80000000
	orr	r0, r0, #0x00800000
	bic	r0, r0, #0xff000000
	rsbne	r0, r0, #0
	tst	r1, #0x80000000
	orr	r1, r1, #0x00800000
	bic	r1, r1, #0xff000000
	rsbne	r1, r1, #0

	@ If exponent == difference, one or both args were denormalized.
	@ Since this is not common case, rescale them off line.
	teq	r2, r3
	beq	LSYM(Lad_d)
LSYM(Lad_x):

	@ Scale down second arg with exponent difference.
	@ Apply shift one bit left to first arg and the rest to second arg
	@ to simplify things later, but only if exponent does not become 0.
	movs	r3, r3, lsr #23
	teqne	r2, #(1 << 23)
	movne	r0, r0, lsl #1
	subne	r2, r2, #(1 << 23)
	subne	r3, r3, #1

	@ Shift second arg into ip, keep leftover bits into r1.
	mov	ip, r1, asr r3
	rsb	r3, r3, #32
	mov	r1, r1, lsl r3

	add	r0, r0, ip		@ the actual addition

	@ We now have a 64 bit result in r0-r1.
	@ Keep absolute value in r0-r1, sign in r3.
	ands	r3, r0, #0x80000000
	bpl	LSYM(Lad_p)
	rsbs	r1, r1, #0
	rsc	r0, r0, #0

	@ Determine how to normalize the result.
LSYM(Lad_p):
	cmp	r0, #0x00800000
	bcc	LSYM(Lad_l)
	cmp	r0, #0x01000000
	bcc	LSYM(Lad_r0)
	cmp	r0, #0x02000000
	bcc	LSYM(Lad_r1)

	@ Result needs to be shifted right.
	movs	r0, r0, lsr #1
	mov	r1, r1, rrx
	add	r2, r2, #(1 << 23)
LSYM(Lad_r1):
	movs	r0, r0, lsr #1
	mov	r1, r1, rrx
	add	r2, r2, #(1 << 23)

	@ Our result is now properly aligned into r0, remaining bits in r1.
	@ Round with MSB of r1. If halfway between two numbers, round towards
	@ LSB of r0 = 0. 
LSYM(Lad_r0):
	add	r0, r0, r1, lsr #31
	teq	r1, #0x80000000
	biceq	r0, r0, #1

	@ Rounding may have added a new MSB.  Adjust exponent.
	@ That MSB will be cleared when exponent is merged below.
	tst	r0, #0x01000000
	addne	r2, r2, #(1 << 23)

	@ Make sure we did not bust our exponent.
	cmp	r2, #(254 << 23)
	bhi	LSYM(Lad_o)

	@ Pack final result together.
LSYM(Lad_e):
	bic	r0, r0, #0x01800000
	orr	r0, r0, r2
	orr	r0, r0, r3
	RET

	@ Result must be shifted left.
	@ No rounding necessary since r1 will always be 0.
LSYM(Lad_l):

#if __ARM_ARCH__ < 5

	movs	ip, r0, lsr #12
	moveq	r0, r0, lsl #12
	subeq	r2, r2, #(12 << 23)
	tst	r0, #0x00ff0000
	moveq	r0, r0, lsl #8
	subeq	r2, r2, #(8 << 23)
	tst	r0, #0x00f00000
	moveq	r0, r0, lsl #4
	subeq	r2, r2, #(4 << 23)
	tst	r0, #0x00c00000
	moveq	r0, r0, lsl #2
	subeq	r2, r2, #(2 << 23)
	tst	r0, #0x00800000
	moveq	r0, r0, lsl #1
	subeq	r2, r2, #(1 << 23)
	cmp	r2, #0
	bgt	LSYM(Lad_e)

#else

	clz	ip, r0
	sub	ip, ip, #8
	mov	r0, r0, lsl ip
	subs	r2, r2, ip, lsl #23
	bgt	LSYM(Lad_e)

#endif

	@ Exponent too small, denormalize result.
	mvn	r2, r2, asr #23
	add	r2, r2, #2
	orr	r0, r3, r0, lsr r2
	RET

	@ Fixup and adjust bit position for denormalized arguments.
	@ Note that r2 must not remain equal to 0.
LSYM(Lad_d):
	teq	r2, #0
	eoreq	r0, r0, #0x00800000
	addeq	r2, r2, #(1 << 23)
	eor	r1, r1, #0x00800000
	subne	r3, r3, #(1 << 23)
	b	LSYM(Lad_x)

	@ Result is x - x = 0, unless x is INF or NAN.
LSYM(Lad_z):
	mov	ip, #0xff000000
	and	r2, r0, ip, lsr #1
	teq	r2, ip, lsr #1
	moveq	r0, ip, asr #2
	movne	r0, #0
	RET

	@ Overflow: return INF.
LSYM(Lad_o):
	orr	r0, r3, #0x7f000000
	orr	r0, r0, #0x00800000
	RET

	@ At least one of r0/r1 is INF/NAN.
	@   if r0 != INF/NAN: return r1 (which is INF/NAN)
	@   if r1 != INF/NAN: return r0 (which is INF/NAN)
	@   if r0 or r1 is NAN: return NAN
	@   if opposite sign: return NAN
	@   return r0 (which is INF or -INF)
LSYM(Lad_i):
	teq	r2, ip, lsr #1
	movne	r0, r1
	teqeq	r3, ip, lsr #1
	RETc(ne)
	movs	r2, r0, lsl #9
	moveqs	r2, r1, lsl #9
	teqeq	r0, r1
	orrne	r0, r3, #0x00400000	@ NAN
	RET

	FUNC_END addsf3
	FUNC_END subsf3

ARM_FUNC_START floatunsisf
	mov	r3, #0
	b	1f

ARM_FUNC_START floatsisf
	ands	r3, r0, #0x80000000
	rsbmi	r0, r0, #0

1:	teq	r0, #0
	RETc(eq)

	mov	r1, #0
	mov	r2, #((127 + 23) << 23)
	tst	r0, #0xfc000000
	beq	LSYM(Lad_p)

	@ We need to scale the value a little before branching to code above.
	tst	r0, #0xf0000000
	movne	r1, r0, lsl #28
	movne	r0, r0, lsr #4
	addne	r2, r2, #(4 << 23)
	tst	r0, #0x0c000000
	beq	LSYM(Lad_p)
	mov	r1, r1, lsr #2
	orr	r1, r1, r0, lsl #30
	mov	r0, r0, lsr #2
	add	r2, r2, #(2 << 23)
	b	LSYM(Lad_p)

	FUNC_END floatsisf
	FUNC_END floatunsisf

#endif /* L_addsubsf3 */

#ifdef L_muldivsf3

ARM_FUNC_START mulsf3

	@ Mask out exponents.
	mov	ip, #0xff000000
	and	r2, r0, ip, lsr #1
	and	r3, r1, ip, lsr #1

	@ Trap any INF/NAN.
	teq	r2, ip, lsr #1
	teqne	r3, ip, lsr #1
	beq	LSYM(Lml_s)

	@ Trap any multiplication by 0.
	bics	ip, r0, #0x80000000
	bicnes	ip, r1, #0x80000000
	beq	LSYM(Lml_z)

	@ Shift exponents right one bit to make room for overflow bit.
	@ If either of them is 0, scale denormalized arguments off line.
	@ Then add both exponents together.
	movs	r2, r2, lsr #1
	teqne	r3, #0
	beq	LSYM(Lml_d)
LSYM(Lml_x):
	add	r2, r2, r3, asr #1

	@ Preserve final sign in r2 along with exponent for now.
	teq	r0, r1
	orrmi	r2, r2, #0x8000

	@ Convert mantissa to unsigned integer.
	bic	r0, r0, #0xff000000
	bic	r1, r1, #0xff000000
	orr	r0, r0, #0x00800000
	orr	r1, r1, #0x00800000

#if __ARM_ARCH__ < 4

	@ Well, no way to make it shorter without the umull instruction.
	@ We must perform that 24 x 24 -> 48 bit multiplication by hand.
	stmfd	sp!, {r4, r5}
	mov	r4, r0, lsr #16
	mov	r5, r1, lsr #16
	bic	r0, r0, #0x00ff0000
	bic	r1, r1, #0x00ff0000
	mul	ip, r4, r5
	mul	r3, r0, r1
	mul	r0, r5, r0
	mla	r0, r4, r1, r0
	adds	r3, r3, r0, lsl #16
	adc	ip, ip, r0, lsr #16
	ldmfd	sp!, {r4, r5}

#else

	umull	r3, ip, r0, r1		@ The actual multiplication.

#endif

	@ Put final sign in r0.
	mov	r0, r2, lsl #16
	bic	r2, r2, #0x8000

	@ Adjust result if one extra MSB appeared.
	@ The LSB may be lost but this never changes the result in this case.
	tst	ip, #(1 << 15)
	addne	r2, r2, #(1 << 22)
	movnes	ip, ip, lsr #1
	movne	r3, r3, rrx

	@ Apply exponent bias, check range for underflow.
	subs	r2, r2, #(127 << 22)
	ble	LSYM(Lml_u)

	@ Scale back to 24 bits with rounding.
	@ r0 contains sign bit already.
	orrs	r0, r0, r3, lsr #23
	adc	r0, r0, ip, lsl #9

	@ If halfway between two numbers, rounding should be towards LSB = 0.
	mov	r3, r3, lsl #9
	teq	r3, #0x80000000
	biceq	r0, r0, #1

	@ Note: rounding may have produced an extra MSB here.
	@ The extra bit is cleared before merging the exponent below.
	tst	r0, #0x01000000
	addne	r2, r2, #(1 << 22)

	@ Check for exponent overflow
	cmp	r2, #(255 << 22)
	bge	LSYM(Lml_o)

	@ Add final exponent.
	bic	r0, r0, #0x01800000
	orr	r0, r0, r2, lsl #1
	RET

	@ Result is 0, but determine sign anyway.
LSYM(Lml_z):
	eor	r0, r0, r1
	bic	r0, r0, #0x7fffffff
	RET

	@ Check if denormalized result is possible, otherwise return signed 0.
LSYM(Lml_u):
	cmn	r2, #(24 << 22)
	RETc(le)

	@ Find out proper shift value.
	mvn	r1, r2, asr #22
	subs	r1, r1, #7
	bgt	LSYM(Lml_ur)

	@ Shift value left, round, etc.
	add	r1, r1, #32
	orrs	r0, r0, r3, lsr r1
	rsb	r1, r1, #32
	adc	r0, r0, ip, lsl r1
	mov	ip, r3, lsl r1
	teq	ip, #0x80000000
	biceq	r0, r0, #1
	RET

	@ Shift value right, round, etc.
	@ Note: r1 must not be 0 otherwise carry does not get set.
LSYM(Lml_ur):
	orrs	r0, r0, ip, lsr r1
	adc	r0, r0, #0
	rsb	r1, r1, #32
	mov	ip, ip, lsl r1
	teq	r3, #0
	teqeq	ip, #0x80000000
	biceq	r0, r0, #1
	RET

	@ One or both arguments are denormalized.
	@ Scale them leftwards and preserve sign bit.
LSYM(Lml_d):
	teq	r2, #0
	and	ip, r0, #0x80000000
1:	moveq	r0, r0, lsl #1
	tsteq	r0, #0x00800000
	subeq	r2, r2, #(1 << 22)
	beq	1b
	orr	r0, r0, ip
	teq	r3, #0
	and	ip, r1, #0x80000000
2:	moveq	r1, r1, lsl #1
	tsteq	r1, #0x00800000
	subeq	r3, r3, #(1 << 23)
	beq	2b
	orr	r1, r1, ip
	b	LSYM(Lml_x)

	@ One or both args are INF or NAN.
LSYM(Lml_s):
	teq	r0, #0x0
	teqne	r1, #0x0
	teqne	r0, #0x80000000
	teqne	r1, #0x80000000
	beq	LSYM(Lml_n)		@ 0 * INF or INF * 0 -> NAN
	teq	r2, ip, lsr #1
	bne	1f
	movs	r2, r0, lsl #9
	bne	LSYM(Lml_n)		@ NAN * <anything> -> NAN
1:	teq	r3, ip, lsr #1
	bne	LSYM(Lml_i)
	movs	r3, r1, lsl #9
	bne	LSYM(Lml_n)		@ <anything> * NAN -> NAN

	@ Result is INF, but we need to determine its sign.
LSYM(Lml_i):
	eor	r0, r0, r1

	@ Overflow: return INF (sign already in r0).
LSYM(Lml_o):
	and	r0, r0, #0x80000000
	orr	r0, r0, #0x7f000000
	orr	r0, r0, #0x00800000
	RET

	@ Return NAN.
LSYM(Lml_n):
	mov	r0, #0x7f000000
	orr	r0, r0, #0x00c00000
	RET

	FUNC_END mulsf3

ARM_FUNC_START divsf3

	@ Mask out exponents.
	mov	ip, #0xff000000
	and	r2, r0, ip, lsr #1
	and	r3, r1, ip, lsr #1

	@ Trap any INF/NAN or zeroes.
	teq	r2, ip, lsr #1
	teqne	r3, ip, lsr #1
	bicnes	ip, r0, #0x80000000
	bicnes	ip, r1, #0x80000000
	beq	LSYM(Ldv_s)

	@ Shift exponents right one bit to make room for overflow bit.
	@ If either of them is 0, scale denormalized arguments off line.
	@ Then substract divisor exponent from dividend''s.
	movs	r2, r2, lsr #1
	teqne	r3, #0
	beq	LSYM(Ldv_d)
LSYM(Ldv_x):
	sub	r2, r2, r3, asr #1

	@ Preserve final sign into ip.
	eor	ip, r0, r1

	@ Convert mantissa to unsigned integer.
	@ Dividend -> r3, divisor -> r1.
	mov	r3, #0x10000000
	movs	r1, r1, lsl #9
	mov	r0, r0, lsl #9
	beq	LSYM(Ldv_1)
	orr	r1, r3, r1, lsr #4
	orr	r3, r3, r0, lsr #4

	@ Initialize r0 (result) with final sign bit.
	and	r0, ip, #0x80000000

	@ Ensure result will land to known bit position.
	cmp	r3, r1
	subcc	r2, r2, #(1 << 22)
	movcc	r3, r3, lsl #1

	@ Apply exponent bias, check range for over/underflow.
	add	r2, r2, #(127 << 22)
	cmn	r2, #(24 << 22)
	RETc(le)
	cmp	r2, #(255 << 22)
	bge	LSYM(Lml_o)

	@ The actual division loop.
	mov	ip, #0x00800000
1:	cmp	r3, r1
	subcs	r3, r3, r1
	orrcs	r0, r0, ip
	cmp	r3, r1, lsr #1
	subcs	r3, r3, r1, lsr #1
	orrcs	r0, r0, ip, lsr #1
	cmp	r3, r1, lsr #2
	subcs	r3, r3, r1, lsr #2
	orrcs	r0, r0, ip, lsr #2
	cmp	r3, r1, lsr #3
	subcs	r3, r3, r1, lsr #3
	orrcs	r0, r0, ip, lsr #3
	movs	r3, r3, lsl #4
	movnes	ip, ip, lsr #4
	bne	1b

	@ Check if denormalized result is needed.
	cmp	r2, #0
	ble	LSYM(Ldv_u)

	@ Apply proper rounding.
	cmp	r3, r1
	addcs	r0, r0, #1
	biceq	r0, r0, #1

	@ Add exponent to result.
	bic	r0, r0, #0x00800000
	orr	r0, r0, r2, lsl #1
	RET

	@ Division by 0x1p*: let''s shortcut a lot of code.
LSYM(Ldv_1):
	and	ip, ip, #0x80000000
	orr	r0, ip, r0, lsr #9
	add	r2, r2, #(127 << 22)
	cmp	r2, #(255 << 22)
	bge	LSYM(Lml_o)
	cmp	r2, #0
	orrgt	r0, r0, r2, lsl #1
	RETc(gt)
	cmn	r2, #(24 << 22)
	movle	r0, ip
	RETc(le)
	orr	r0, r0, #0x00800000
	mov	r3, #0

	@ Result must be denormalized: prepare parameters to use code above.
	@ r3 already contains remainder for rounding considerations.
LSYM(Ldv_u):
	bic	ip, r0, #0x80000000
	and	r0, r0, #0x80000000
	mvn	r1, r2, asr #22
	add	r1, r1, #2
	b	LSYM(Lml_ur)

	@ One or both arguments are denormalized.
	@ Scale them leftwards and preserve sign bit.
LSYM(Ldv_d):
	teq	r2, #0
	and	ip, r0, #0x80000000
1:	moveq	r0, r0, lsl #1
	tsteq	r0, #0x00800000
	subeq	r2, r2, #(1 << 22)
	beq	1b
	orr	r0, r0, ip
	teq	r3, #0
	and	ip, r1, #0x80000000
2:	moveq	r1, r1, lsl #1
	tsteq	r1, #0x00800000
	subeq	r3, r3, #(1 << 23)
	beq	2b
	orr	r1, r1, ip
	b	LSYM(Ldv_x)

	@ One or both arguments is either INF, NAN or zero.
LSYM(Ldv_s):
	mov	ip, #0xff000000
	teq	r2, ip, lsr #1
	teqeq	r3, ip, lsr #1
	beq	LSYM(Lml_n)		@ INF/NAN / INF/NAN -> NAN
	teq	r2, ip, lsr #1
	bne	1f
	movs	r2, r0, lsl #9
	bne	LSYM(Lml_n)		@ NAN / <anything> -> NAN
	b	LSYM(Lml_i)		@ INF / <anything> -> INF
1:	teq	r3, ip, lsr #1
	bne	2f
	movs	r3, r1, lsl #9
	bne	LSYM(Lml_n)		@ <anything> / NAN -> NAN
	b	LSYM(Lml_z)		@ <anything> / INF -> 0
2:	@ One or both arguments are 0.
	bics	r2, r0, #0x80000000
	bne	LSYM(Lml_i)		@ <non_zero> / 0 -> INF
	bics	r3, r1, #0x80000000
	bne	LSYM(Lml_z)		@ 0 / <non_zero> -> 0
	b	LSYM(Lml_n)		@ 0 / 0 -> NAN

	FUNC_END divsf3

#endif /* L_muldivsf3 */

#ifdef L_cmpsf2

ARM_FUNC_START gtsf2
ARM_FUNC_ALIAS gesf2 gtsf2
	mov	r3, #-1
	b	1f

ARM_FUNC_START ltsf2
ARM_FUNC_ALIAS lesf2 ltsf2
	mov	r3, #1
	b	1f

ARM_FUNC_START cmpsf2
ARM_FUNC_ALIAS nesf2 cmpsf2
ARM_FUNC_ALIAS eqsf2 cmpsf2
	mov	r3, #1			@ how should we specify unordered here?

1:	@ Trap any INF/NAN first.
	mov	ip, #0xff000000
	and	r2, r1, ip, lsr #1
	teq	r2, ip, lsr #1
	and	r2, r0, ip, lsr #1
	teqne	r2, ip, lsr #1
	beq	3f

	@ Test for equality.
	@ Note that 0.0 is equal to -0.0.
2:	orr	r3, r0, r1
	bics	r3, r3, #0x80000000	@ either 0.0 or -0.0
	teqne	r0, r1			@ or both the same
	moveq	r0, #0
	RETc(eq)

	@ Check for sign difference.  The N flag is set if it is the case.
	@ If so, return sign of r0.
	movmi	r0, r0, asr #31
	orrmi	r0, r0, #1
	RETc(mi)

	@ Compare exponents.
	and	r3, r1, ip, lsr #1
	cmp	r2, r3

	@ Compare mantissa if exponents are equal
	moveq	r0, r0, lsl #9
	cmpeq	r0, r1, lsl #9
	movcs	r0, r1, asr #31
	mvncc	r0, r1, asr #31
	orr	r0, r0, #1
	RET

	@ Look for a NAN. 
3:	and	r2, r1, ip, lsr #1
	teq	r2, ip, lsr #1
	bne	4f
	movs	r2, r1, lsl #9
	bne	5f			@ r1 is NAN
4:	and	r2, r0, ip, lsr #1
	teq	r2, ip, lsr #1
	bne	2b
	movs	ip, r0, lsl #9
	beq	2b			@ r0 is not NAN
5:	mov	r0, r3			@ return unordered code from r3.
	RET

	FUNC_END gesf2
	FUNC_END gtsf2
	FUNC_END lesf2
	FUNC_END ltsf2
	FUNC_END nesf2
	FUNC_END eqsf2
	FUNC_END cmpsf2

#endif /* L_cmpsf2 */

#ifdef L_unordsf2

ARM_FUNC_START unordsf2
	mov	ip, #0xff000000
	and	r2, r1, ip, lsr #1
	teq	r2, ip, lsr #1
	bne	1f
	movs	r2, r1, lsl #9
	bne	3f			@ r1 is NAN
1:	and	r2, r0, ip, lsr #1
	teq	r2, ip, lsr #1
	bne	2f
	movs	r2, r0, lsl #9
	bne	3f			@ r0 is NAN
2:	mov	r0, #0			@ arguments are ordered.
	RET
3:	mov	r0, #1			@ arguments are unordered.
	RET

	FUNC_END unordsf2

#endif /* L_unordsf2 */

#ifdef L_fixsfsi

ARM_FUNC_START fixsfsi
	movs	r0, r0, lsl #1
	RETc(eq)			@ value is 0.

	mov	r1, r1, rrx		@ preserve C flag (the actual sign)

	@ check exponent range.
	and	r2, r0, #0xff000000
	cmp	r2, #(127 << 24)
	movcc	r0, #0			@ value is too small
	RETc(cc)
	cmp	r2, #((127 + 31) << 24)
	bcs	1f			@ value is too large

	mov	r0, r0, lsl #7
	orr	r0, r0, #0x80000000
	mov	r2, r2, lsr #24
	rsb	r2, r2, #(127 + 31)
	tst	r1, #0x80000000		@ the sign bit
	mov	r0, r0, lsr r2
	rsbne	r0, r0, #0
	RET

1:	teq	r2, #0xff000000
	bne	2f
	movs	r0, r0, lsl #8
	bne	3f			@ r0 is NAN.
2:	ands	r0, r1, #0x80000000	@ the sign bit
	moveq	r0, #0x7fffffff		@ the maximum signed positive si
	RET

3:	mov	r0, #0			@ What should we convert NAN to?
	RET

	FUNC_END fixsfsi

#endif /* L_fixsfsi */

#ifdef L_fixunssfsi

ARM_FUNC_START fixunssfsi
	movs	r0, r0, lsl #1
	movcss	r0, #0			@ value is negative...
	RETc(eq)			@ ... or 0.


	@ check exponent range.
	and	r2, r0, #0xff000000
	cmp	r2, #(127 << 24)
	movcc	r0, #0			@ value is too small
	RETc(cc)
	cmp	r2, #((127 + 32) << 24)
	bcs	1f			@ value is too large

	mov	r0, r0, lsl #7
	orr	r0, r0, #0x80000000
	mov	r2, r2, lsr #24
	rsb	r2, r2, #(127 + 31)
	mov	r0, r0, lsr r2
	RET

1:	teq	r2, #0xff000000
	bne	2f
	movs	r0, r0, lsl #8
	bne	3f			@ r0 is NAN.
2:	mov	r0, #0xffffffff		@ maximum unsigned si
	RET

3:	mov	r0, #0			@ What should we convert NAN to?
	RET

	FUNC_END fixunssfsi

#endif /* L_fixunssfsi */
