/* ieee754-df.S double-precision floating point support for ARM

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
 * For slightly simpler code please see the single precision version
 * of this file.
 * 
 * Only the default rounding mode is intended for best performances.
 * Exceptions aren't supported yet, but that can be added quite easily
 * if necessary without impacting performances.
 */


@ For FPA, float words are always big-endian.
@ For VFP, floats words follow the memory system mode.
#if defined(__VFP_FP__) && !defined(__ARMEB__)
#define xl r0
#define xh r1
#define yl r2
#define yh r3
#else
#define xh r0
#define xl r1
#define yh r2
#define yl r3
#endif


#ifdef L_negdf2

ARM_FUNC_START negdf2
	@ flip sign bit
	eor	xh, xh, #0x80000000
	RET

	FUNC_END negdf2

#endif

#ifdef L_addsubdf3

ARM_FUNC_START subdf3
	@ flip sign bit of second arg
	eor	yh, yh, #0x80000000
#if defined(__thumb__) && !defined(__THUMB_INTERWORK__)
	b	1f			@ Skip Thumb-code prologue
#endif

ARM_FUNC_START adddf3

1:	@ Compare both args, return zero if equal but the sign.
	teq	xl, yl
	eoreq	ip, xh, yh
	teqeq	ip, #0x80000000
	beq	LSYM(Lad_z)

	@ If first arg is 0 or -0, return second arg.
	@ If second arg is 0 or -0, return first arg.
	orrs	ip, xl, xh, lsl #1
	moveq	xl, yl
	moveq	xh, yh
	orrnes	ip, yl, yh, lsl #1
	RETc(eq)

	stmfd	sp!, {r4, r5, lr}

	@ Mask out exponents.
	mov	ip, #0x7f000000
	orr	ip, ip, #0x00f00000
	and	r4, xh, ip
	and	r5, yh, ip

	@ If either of them is 0x7ff, result will be INF or NAN
	teq	r4, ip
	teqne	r5, ip
	beq	LSYM(Lad_i)

	@ Compute exponent difference.  Make largest exponent in r4,
	@ corresponding arg in xh-xl, and positive exponent difference in r5.
	subs	r5, r5, r4
	rsblt	r5, r5, #0
	ble	1f
	add	r4, r4, r5
	eor	yl, xl, yl
	eor	yh, xh, yh
	eor	xl, yl, xl
	eor	xh, yh, xh
	eor	yl, xl, yl
	eor	yh, xh, yh
1:

	@ If exponent difference is too large, return largest argument
	@ already in xh-xl.  We need up to 54 bit to handle proper rounding
	@ of 0x1p54 - 1.1.
	cmp	r5, #(54 << 20)
	RETLDM	"r4, r5" hi

	@ Convert mantissa to signed integer.
	tst	xh, #0x80000000
	bic	xh, xh, ip, lsl #1
	orr	xh, xh, #0x00100000
	beq	1f
	rsbs	xl, xl, #0
	rsc	xh, xh, #0
1:
	tst	yh, #0x80000000
	bic	yh, yh, ip, lsl #1
	orr	yh, yh, #0x00100000
	beq	1f
	rsbs	yl, yl, #0
	rsc	yh, yh, #0
1:
	@ If exponent == difference, one or both args were denormalized.
	@ Since this is not common case, rescale them off line.
	teq	r4, r5
	beq	LSYM(Lad_d)
LSYM(Lad_x):
	@ Scale down second arg with exponent difference.
	@ Apply shift one bit left to first arg and the rest to second arg
	@ to simplify things later, but only if exponent does not become 0.
	mov	ip, #0
	movs	r5, r5, lsr #20
	beq	3f
	teq	r4, #(1 << 20)
	beq	1f
	movs	xl, xl, lsl #1
	adc	xh, ip, xh, lsl #1
	sub	r4, r4, #(1 << 20)
	subs	r5, r5, #1
	beq	3f

	@ Shift yh-yl right per r5, keep leftover bits into ip.
1:	rsbs	lr, r5, #32
	blt	2f
	mov	ip, yl, lsl lr
	mov	yl, yl, lsr r5
	orr	yl, yl, yh, lsl lr
	mov	yh, yh, asr r5
	b	3f
2:	sub	r5, r5, #32
	add	lr, lr, #32
	cmp	yl, #1
	adc	ip, ip, yh, lsl lr
	mov	yl, yh, asr r5
	mov	yh, yh, asr #32
3:
	@ the actual addition
	adds	xl, xl, yl
	adc	xh, xh, yh

	@ We now have a result in xh-xl-ip.
	@ Keep absolute value in xh-xl-ip, sign in r5.
	ands	r5, xh, #0x80000000
	bpl	LSYM(Lad_p)
	rsbs	ip, ip, #0
	rscs	xl, xl, #0
	rsc	xh, xh, #0

	@ Determine how to normalize the result.
LSYM(Lad_p):
	cmp	xh, #0x00100000
	bcc	LSYM(Lad_l)
	cmp	xh, #0x00200000
	bcc	LSYM(Lad_r0)
	cmp	xh, #0x00400000
	bcc	LSYM(Lad_r1)

	@ Result needs to be shifted right.
	movs	xh, xh, lsr #1
	movs	xl, xl, rrx
	movs	ip, ip, rrx
	orrcs	ip, ip, #1
	add	r4, r4, #(1 << 20)
LSYM(Lad_r1):
	movs	xh, xh, lsr #1
	movs	xl, xl, rrx
	movs	ip, ip, rrx
	orrcs	ip, ip, #1
	add	r4, r4, #(1 << 20)

	@ Our result is now properly aligned into xh-xl, remaining bits in ip.
	@ Round with MSB of ip. If halfway between two numbers, round towards
	@ LSB of xl = 0.
LSYM(Lad_r0):
	adds	xl, xl, ip, lsr #31
	adc	xh, xh, #0
	teq	ip, #0x80000000
	biceq	xl, xl, #1

	@ One extreme rounding case may add a new MSB.  Adjust exponent.
	@ That MSB will be cleared when exponent is merged below. 
	tst	xh, #0x00200000
	addne	r4, r4, #(1 << 20)

	@ Make sure we did not bust our exponent.
	adds	ip, r4, #(1 << 20)
	bmi	LSYM(Lad_o)

	@ Pack final result together.
LSYM(Lad_e):
	bic	xh, xh, #0x00300000
	orr	xh, xh, r4
	orr	xh, xh, r5
	RETLDM	"r4, r5"

LSYM(Lad_l):
	@ Result must be shifted left and exponent adjusted.
	@ No rounding necessary since ip will always be 0.
#if __ARM_ARCH__ < 5

	teq	xh, #0
	movne	r3, #-11
	moveq	r3, #21
	moveq	xh, xl
	moveq	xl, #0
	mov	r2, xh
	movs	ip, xh, lsr #16
	moveq	r2, r2, lsl #16
	addeq	r3, r3, #16
	tst	r2, #0xff000000
	moveq	r2, r2, lsl #8
	addeq	r3, r3, #8
	tst	r2, #0xf0000000
	moveq	r2, r2, lsl #4
	addeq	r3, r3, #4
	tst	r2, #0xc0000000
	moveq	r2, r2, lsl #2
	addeq	r3, r3, #2
	tst	r2, #0x80000000
	addeq	r3, r3, #1

#else

	teq	xh, #0
	moveq	xh, xl
	moveq	xl, #0
	clz	r3, xh
	addeq	r3, r3, #32
	sub	r3, r3, #11

#endif

	@ determine how to shift the value.
	subs	r2, r3, #32
	bge	2f
	adds	r2, r2, #12
	ble	1f

	@ shift value left 21 to 31 bits, or actually right 11 to 1 bits
	@ since a register switch happened above.
	add	ip, r2, #20
	rsb	r2, r2, #12
	mov	xl, xh, lsl ip
	mov	xh, xh, lsr r2
	b	3f

	@ actually shift value left 1 to 20 bits, which might also represent
	@ 32 to 52 bits if counting the register switch that happened earlier.
1:	add	r2, r2, #20
2:	rsble	ip, r2, #32
	mov	xh, xh, lsl r2
	orrle	xh, xh, xl, lsr ip
	movle	xl, xl, lsl r2

	@ adjust exponent accordingly.
3:	subs	r4, r4, r3, lsl #20
	bgt	LSYM(Lad_e)

	@ Exponent too small, denormalize result.
	@ Find out proper shift value.
	mvn	r4, r4, asr #20
	subs	r4, r4, #30
	bge	2f
	adds	r4, r4, #12
	bgt	1f

	@ shift result right of 1 to 20 bits, sign is in r5.
	add	r4, r4, #20
	rsb	r2, r4, #32
	mov	xl, xl, lsr r4
	orr	xl, xl, xh, lsl r2
	orr	xh, r5, xh, lsr r4
	RETLDM	"r4, r5"

	@ shift result right of 21 to 31 bits, or left 11 to 1 bits after
	@ a register switch from xh to xl.
1:	rsb	r4, r4, #12
	rsb	r2, r4, #32
	mov	xl, xl, lsr r2
	orr	xl, xl, xh, lsl r4
	mov	xh, r5
	RETLDM	"r4, r5"

	@ Shift value right of 32 to 64 bits, or 0 to 32 bits after a switch
	@ from xh to xl.
2:	mov	xl, xh, lsr r4
	mov	xh, r5
	RETLDM	"r4, r5"

	@ Adjust exponents for denormalized arguments.
LSYM(Lad_d):
	teq	r4, #0
	eoreq	xh, xh, #0x00100000
	addeq	r4, r4, #(1 << 20)
	eor	yh, yh, #0x00100000
	subne	r5, r5, #(1 << 20)
	b	LSYM(Lad_x)

	@ Result is x - x = 0, unless x = INF or NAN.
LSYM(Lad_z):
	sub	ip, ip, #0x00100000	@ ip becomes 0x7ff00000
	and	r2, xh, ip
	teq	r2, ip
	orreq	xh, ip, #0x00080000
	movne	xh, #0
	mov	xl, #0
	RET

	@ Overflow: return INF.
LSYM(Lad_o):
	orr	xh, r5, #0x7f000000
	orr	xh, xh, #0x00f00000
	mov	xl, #0
	RETLDM	"r4, r5"

	@ At least one of x or y is INF/NAN.
	@   if xh-xl != INF/NAN: return yh-yl (which is INF/NAN)
	@   if yh-yl != INF/NAN: return xh-xl (which is INF/NAN)
	@   if either is NAN: return NAN
	@   if opposite sign: return NAN
	@   return xh-xl (which is INF or -INF)
LSYM(Lad_i):
	teq	r4, ip
	movne	xh, yh
	movne	xl, yl
	teqeq	r5, ip
	RETLDM	"r4, r5" ne

	orrs	r4, xl, xh, lsl #12
	orreqs	r4, yl, yh, lsl #12
	teqeq	xh, yh
	orrne	xh, r5, #0x00080000
	movne	xl, #0
	RETLDM	"r4, r5"

	FUNC_END subdf3
	FUNC_END adddf3

ARM_FUNC_START floatunsidf
	teq	r0, #0
	moveq	r1, #0
	RETc(eq)
	stmfd	sp!, {r4, r5, lr}
	mov	r4, #(0x400 << 20)	@ initial exponent
	add	r4, r4, #((52-1) << 20)
	mov	r5, #0			@ sign bit is 0
	mov	xl, r0
	mov	xh, #0
	b	LSYM(Lad_l)

	FUNC_END floatunsidf

ARM_FUNC_START floatsidf
	teq	r0, #0
	moveq	r1, #0
	RETc(eq)
	stmfd	sp!, {r4, r5, lr}
	mov	r4, #(0x400 << 20)	@ initial exponent
	add	r4, r4, #((52-1) << 20)
	ands	r5, r0, #0x80000000	@ sign bit in r5
	rsbmi	r0, r0, #0		@ absolute value
	mov	xl, r0
	mov	xh, #0
	b	LSYM(Lad_l)

	FUNC_END floatsidf

ARM_FUNC_START extendsfdf2
	movs	r2, r0, lsl #1
	beq	1f			@ value is 0.0 or -0.0
	mov	xh, r2, asr #3		@ stretch exponent
	mov	xh, xh, rrx		@ retrieve sign bit
	mov	xl, r2, lsl #28		@ retrieve remaining bits
	ands	r2, r2, #0xff000000	@ isolate exponent
	beq	2f			@ exponent was 0 but not mantissa
	teq	r2, #0xff000000		@ check if INF or NAN
	eorne	xh, xh, #0x38000000	@ fixup exponent otherwise.
	RET

1:	mov	xh, r0
	mov	xl, #0
	RET

2:	@ value was denormalized.  We can normalize it now.
	stmfd	sp!, {r4, r5, lr}
	mov	r4, #(0x380 << 20)	@ setup corresponding exponent
	add	r4, r4, #(1 << 20)
	and	r5, xh, #0x80000000	@ move sign bit in r5
	bic	xh, xh, #0x80000000
	b	LSYM(Lad_l)

	FUNC_END extendsfdf2

#endif /* L_addsubdf3 */

#ifdef L_muldivdf3

ARM_FUNC_START muldf3

	stmfd	sp!, {r4, r5, r6, lr}

	@ Mask out exponents.
	mov	ip, #0x7f000000
	orr	ip, ip, #0x00f00000
	and	r4, xh, ip
	and	r5, yh, ip

	@ Trap any INF/NAN.
	teq	r4, ip
	teqne	r5, ip
	beq	LSYM(Lml_s)

	@ Trap any multiplication by 0.
	orrs	r6, xl, xh, lsl #1
	orrnes	r6, yl, yh, lsl #1
	beq	LSYM(Lml_z)

	@ Shift exponents right one bit to make room for overflow bit.
	@ If either of them is 0, scale denormalized arguments off line.
	@ Then add both exponents together.
	movs	r4, r4, lsr #1
	teqne	r5, #0
	beq	LSYM(Lml_d)
LSYM(Lml_x):
	add	r4, r4, r5, asr #1

	@ Preserve final sign in r4 along with exponent for now.
	teq	xh, yh
	orrmi	r4, r4, #0x8000

	@ Convert mantissa to unsigned integer.
	bic	xh, xh, ip, lsl #1
	bic	yh, yh, ip, lsl #1
	orr	xh, xh, #0x00100000
	orr	yh, yh, #0x00100000

#if __ARM_ARCH__ < 4

	@ Well, no way to make it shorter without the umull instruction.
	@ We must perform that 53 x 53 bit multiplication by hand.
	stmfd	sp!, {r7, r8, r9, sl, fp}
	mov	r7, xl, lsr #16
	mov	r8, yl, lsr #16
	mov	r9, xh, lsr #16
	mov	sl, yh, lsr #16
	bic	xl, xl, r7, lsl #16
	bic	yl, yl, r8, lsl #16
	bic	xh, xh, r9, lsl #16
	bic	yh, yh, sl, lsl #16
	mul	ip, xl, yl
	mul	fp, xl, r8
	mov	lr, #0
	adds	ip, ip, fp, lsl #16
	adc	lr, lr, fp, lsr #16
	mul	fp, r7, yl
	adds	ip, ip, fp, lsl #16
	adc	lr, lr, fp, lsr #16
	mul	fp, xl, sl
	mov	r5, #0
	adds	lr, lr, fp, lsl #16
	adc	r5, r5, fp, lsr #16
	mul	fp, r7, yh
	adds	lr, lr, fp, lsl #16
	adc	r5, r5, fp, lsr #16
	mul	fp, xh, r8
	adds	lr, lr, fp, lsl #16
	adc	r5, r5, fp, lsr #16
	mul	fp, r9, yl
	adds	lr, lr, fp, lsl #16
	adc	r5, r5, fp, lsr #16
	mul	fp, xh, sl
	mul	r6, r9, sl
	adds	r5, r5, fp, lsl #16
	adc	r6, r6, fp, lsr #16
	mul	fp, r9, yh
	adds	r5, r5, fp, lsl #16
	adc	r6, r6, fp, lsr #16
	mul	fp, xl, yh
	adds	lr, lr, fp
	mul	fp, r7, sl
	adcs	r5, r5, fp
	mul	fp, xh, yl
	adc	r6, r6, #0
	adds	lr, lr, fp
	mul	fp, r9, r8
	adcs	r5, r5, fp
	mul	fp, r7, r8
	adc	r6, r6, #0
	adds	lr, lr, fp
	mul	fp, xh, yh
	adcs	r5, r5, fp
	adc	r6, r6, #0
	ldmfd	sp!, {r7, r8, r9, sl, fp}

#else

	@ Here is the actual multiplication: 53 bits * 53 bits -> 106 bits.
	umull	ip, lr, xl, yl
	mov	r5, #0
	umlal	lr, r5, xl, yh
	umlal	lr, r5, xh, yl
	mov	r6, #0
	umlal	r5, r6, xh, yh

#endif

	@ The LSBs in ip are only significant for the final rounding.
	@ Fold them into one bit of lr.
	teq	ip, #0
	orrne	lr, lr, #1

	@ Put final sign in xh.
	mov	xh, r4, lsl #16
	bic	r4, r4, #0x8000

	@ Adjust result if one extra MSB appeared (one of four times).
	tst	r6, #(1 << 9)
	beq	1f
	add	r4, r4, #(1 << 19)
	movs	r6, r6, lsr #1
	movs	r5, r5, rrx
	movs	lr, lr, rrx
	orrcs	lr, lr, #1
1:
	@ Scale back to 53 bits.
	@ xh contains sign bit already.
	orr	xh, xh, r6, lsl #12
	orr	xh, xh, r5, lsr #20
	mov	xl, r5, lsl #12
	orr	xl, xl, lr, lsr #20

	@ Apply exponent bias, check range for underflow.
	sub	r4, r4, #0x00f80000
	subs	r4, r4, #0x1f000000
	ble	LSYM(Lml_u)

	@ Round the result.
	movs	lr, lr, lsl #12
	bpl	1f
	adds	xl, xl, #1
	adc	xh, xh, #0
	teq	lr, #0x80000000
	biceq	xl, xl, #1

	@ Rounding may have produced an extra MSB here.
	@ The extra bit is cleared before merging the exponent below.
	tst	xh, #0x00200000
	addne	r4, r4, #(1 << 19)
1:
	@ Check exponent for overflow.
	adds	ip, r4, #(1 << 19)
	tst	ip, #(1 << 30)
	bne	LSYM(Lml_o)

	@ Add final exponent.
	bic	xh, xh, #0x00300000
	orr	xh, xh, r4, lsl #1
	RETLDM	"r4, r5, r6"

	@ Result is 0, but determine sign anyway.
LSYM(Lml_z):
	eor	xh, xh, yh
LSYM(Ldv_z):
	bic	xh, xh, #0x7fffffff
	mov	xl, #0
	RETLDM	"r4, r5, r6"

	@ Check if denormalized result is possible, otherwise return signed 0.
LSYM(Lml_u):
	cmn	r4, #(53 << 19)
	movle	xl, #0
	bicle	xh, xh, #0x7fffffff
	RETLDM	"r4, r5, r6" le

	@ Find out proper shift value.
LSYM(Lml_r):
	mvn	r4, r4, asr #19
	subs	r4, r4, #30
	bge	2f
	adds	r4, r4, #12
	bgt	1f

	@ shift result right of 1 to 20 bits, preserve sign bit, round, etc.
	add	r4, r4, #20
	rsb	r5, r4, #32
	mov	r3, xl, lsl r5
	mov	xl, xl, lsr r4
	orr	xl, xl, xh, lsl r5
	movs	xh, xh, lsl #1
	mov	xh, xh, lsr r4
	mov	xh, xh, rrx
	adds	xl, xl, r3, lsr #31
	adc	xh, xh, #0
	teq	lr, #0
	teqeq	r3, #0x80000000
	biceq	xl, xl, #1
	RETLDM	"r4, r5, r6"

	@ shift result right of 21 to 31 bits, or left 11 to 1 bits after
	@ a register switch from xh to xl. Then round.
1:	rsb	r4, r4, #12
	rsb	r5, r4, #32
	mov	r3, xl, lsl r4
	mov	xl, xl, lsr r5
	orr	xl, xl, xh, lsl r4
	bic	xh, xh, #0x7fffffff
	adds	xl, xl, r3, lsr #31
	adc	xh, xh, #0
	teq	lr, #0
	teqeq	r3, #0x80000000
	biceq	xl, xl, #1
	RETLDM	"r4, r5, r6"

	@ Shift value right of 32 to 64 bits, or 0 to 32 bits after a switch
	@ from xh to xl.  Leftover bits are in r3-r6-lr for rounding.
2:	rsb	r5, r4, #32
	mov	r6, xl, lsl r5
	mov	r3, xl, lsr r4
	orr	r3, r3, xh, lsl r5
	mov	xl, xh, lsr r4
	bic	xh, xh, #0x7fffffff
	bic	xl, xl, xh, lsr r4
	add	xl, xl, r3, lsr #31
	orrs	r6, r6, lr
	teqeq	r3, #0x80000000
	biceq	xl, xl, #1
	RETLDM	"r4, r5, r6"

	@ One or both arguments are denormalized.
	@ Scale them leftwards and preserve sign bit.
LSYM(Lml_d):
	mov	lr, #0
	teq	r4, #0
	bne	2f
	and	r6, xh, #0x80000000
1:	movs	xl, xl, lsl #1
	adc	xh, lr, xh, lsl #1
	tst	xh, #0x00100000
	subeq	r4, r4, #(1 << 19)
	beq	1b
	orr	xh, xh, r6
	teq	r5, #0
	bne	LSYM(Lml_x)
2:	and	r6, yh, #0x80000000
3:	movs	yl, yl, lsl #1
	adc	yh, lr, yh, lsl #1
	tst	yh, #0x00100000
	subeq	r5, r5, #(1 << 20)
	beq	3b
	orr	yh, yh, r6
	b	LSYM(Lml_x)

	@ One or both args are INF or NAN.
LSYM(Lml_s):
	orrs	r6, xl, xh, lsl #1
	orrnes	r6, yl, yh, lsl #1
	beq	LSYM(Lml_n)		@ 0 * INF or INF * 0 -> NAN
	teq	r4, ip
	bne	1f
	orrs	r6, xl, xh, lsl #12
	bne	LSYM(Lml_n)		@ NAN * <anything> -> NAN
1:	teq	r5, ip
	bne	LSYM(Lml_i)
	orrs	r6, yl, yh, lsl #12
	bne	LSYM(Lml_n)		@ <anything> * NAN -> NAN

	@ Result is INF, but we need to determine its sign.
LSYM(Lml_i):
	eor	xh, xh, yh

	@ Overflow: return INF (sign already in xh).
LSYM(Lml_o):
	and	xh, xh, #0x80000000
	orr	xh, xh, #0x7f000000
	orr	xh, xh, #0x00f00000
	mov	xl, #0
	RETLDM	"r4, r5, r6"

	@ Return NAN.
LSYM(Lml_n):
	mov	xh, #0x7f000000
	orr	xh, xh, #0x00f80000
	RETLDM	"r4, r5, r6"

	FUNC_END muldf3

ARM_FUNC_START divdf3

	stmfd	sp!, {r4, r5, r6, lr}

	@ Mask out exponents.
	mov	ip, #0x7f000000
	orr	ip, ip, #0x00f00000
	and	r4, xh, ip
	and	r5, yh, ip

	@ Trap any INF/NAN or zeroes.
	teq	r4, ip
	teqne	r5, ip
	orrnes	r6, xl, xh, lsl #1
	orrnes	r6, yl, yh, lsl #1
	beq	LSYM(Ldv_s)

	@ Shift exponents right one bit to make room for overflow bit.
	@ If either of them is 0, scale denormalized arguments off line.
	@ Then substract divisor exponent from dividend''s.
	movs	r4, r4, lsr #1
	teqne	r5, #0
	beq	LSYM(Ldv_d)
LSYM(Ldv_x):
	sub	r4, r4, r5, asr #1

	@ Preserve final sign into lr.
	eor	lr, xh, yh

	@ Convert mantissa to unsigned integer.
	@ Dividend -> r5-r6, divisor -> yh-yl.
	mov	r5, #0x10000000
	mov	yh, yh, lsl #12
	orr	yh, r5, yh, lsr #4
	orr	yh, yh, yl, lsr #24
	movs	yl, yl, lsl #8
	mov	xh, xh, lsl #12
	teqeq	yh, r5
	beq	LSYM(Ldv_1)
	orr	r5, r5, xh, lsr #4
	orr	r5, r5, xl, lsr #24
	mov	r6, xl, lsl #8

	@ Initialize xh with final sign bit.
	and	xh, lr, #0x80000000

	@ Ensure result will land to known bit position.
	cmp	r5, yh
	cmpeq	r6, yl
	bcs	1f
	sub	r4, r4, #(1 << 19)
	movs	yh, yh, lsr #1
	mov	yl, yl, rrx
1:
	@ Apply exponent bias, check range for over/underflow.
	add	r4, r4, #0x1f000000
	add	r4, r4, #0x00f80000
	cmn	r4, #(53 << 19)
	ble	LSYM(Ldv_z)
	cmp	r4, ip, lsr #1
	bge	LSYM(Lml_o)

	@ Perform first substraction to align result to a nibble.
	subs	r6, r6, yl
	sbc	r5, r5, yh
	movs	yh, yh, lsr #1
	mov	yl, yl, rrx
	mov	xl, #0x00100000
	mov	ip, #0x00080000

	@ The actual division loop.
1:	subs	lr, r6, yl
	sbcs	lr, r5, yh
	subcs	r6, r6, yl
	movcs	r5, lr
	orrcs	xl, xl, ip
	movs	yh, yh, lsr #1
	mov	yl, yl, rrx
	subs	lr, r6, yl
	sbcs	lr, r5, yh
	subcs	r6, r6, yl
	movcs	r5, lr
	orrcs	xl, xl, ip, lsr #1
	movs	yh, yh, lsr #1
	mov	yl, yl, rrx
	subs	lr, r6, yl
	sbcs	lr, r5, yh
	subcs	r6, r6, yl
	movcs	r5, lr
	orrcs	xl, xl, ip, lsr #2
	movs	yh, yh, lsr #1
	mov	yl, yl, rrx
	subs	lr, r6, yl
	sbcs	lr, r5, yh
	subcs	r6, r6, yl
	movcs	r5, lr
	orrcs	xl, xl, ip, lsr #3

	orrs	lr, r5, r6
	beq	2f
	mov	r5, r5, lsl #4
	orr	r5, r5, r6, lsr #28
	mov	r6, r6, lsl #4
	mov	yh, yh, lsl #3
	orr	yh, yh, yl, lsr #29
	mov	yl, yl, lsl #3
	movs	ip, ip, lsr #4
	bne	1b

	@ We are done with a word of the result.
	@ Loop again for the low word if this pass was for the high word.
	tst	xh, #0x00100000
	bne	3f
	orr	xh, xh, xl
	mov	xl, #0
	mov	ip, #0x80000000
	b	1b
2:
	@ Be sure result starts in the high word.
	tst	xh, #0x00100000
	orreq	xh, xh, xl
	moveq	xl, #0
3:
	@ Check if denormalized result is needed.
	cmp	r4, #0
	ble	LSYM(Ldv_u)

	@ Apply proper rounding.
	subs	ip, r5, yh
	subeqs	ip, r6, yl
	adcs	xl, xl, #0
	adc	xh, xh, #0
	teq	ip, #0
	biceq	xl, xl, #1

	@ Add exponent to result.
	bic	xh, xh, #0x00100000
	orr	xh, xh, r4, lsl #1
	RETLDM	"r4, r5, r6"

	@ Division by 0x1p*: shortcut a lot of code.
LSYM(Ldv_1):
	and	lr, lr, #0x80000000
	orr	xh, lr, xh, lsr #12
	add	r4, r4, #0x1f000000
	add	r4, r4, #0x00f80000
	cmp	r4, ip, lsr #1
	bge	LSYM(Lml_o)
	cmp	r4, #0
	orrgt	xh, xh, r4, lsl #1
	RETLDM	"r4, r5, r6" gt

	cmn	r4, #(53 << 19)
	ble	LSYM(Ldv_z)
	orr	xh, xh, #0x00100000
	mov	lr, #0
	b	LSYM(Lml_r)

	@ Result must be denormalized: put remainder in lr for
	@ rounding considerations.
LSYM(Ldv_u):
	orr	lr, r5, r6
	b	LSYM(Lml_r)

	@ One or both arguments are denormalized.
	@ Scale them leftwards and preserve sign bit.
LSYM(Ldv_d):
	mov	lr, #0
	teq	r4, #0
	bne	2f
	and	r6, xh, #0x80000000
1:	movs	xl, xl, lsl #1
	adc	xh, lr, xh, lsl #1
	tst	xh, #0x00100000
	subeq	r4, r4, #(1 << 19)
	beq	1b
	orr	xh, xh, r6
	teq	r5, #0
	bne	LSYM(Ldv_x)
2:	and	r6, yh, #0x80000000
3:	movs	yl, yl, lsl #1
	adc	yh, lr, yh, lsl #1
	tst	yh, #0x00100000
	subeq	r5, r5, #(1 << 20)
	beq	3b
	orr	yh, yh, r6
	b	LSYM(Ldv_x)

	@ One or both arguments is either INF, NAN or zero.
LSYM(Ldv_s):
	teq	r4, ip
	teqeq	r5, ip
	beq	LSYM(Lml_n)		@ INF/NAN / INF/NAN -> NAN
	teq	r4, ip
	bne	1f
	orrs	r4, xl, xh, lsl #12
	bne	LSYM(Lml_n)		@ NAN / <anything> -> NAN
	b	LSYM(Lml_i)		@ INF / <anything> -> INF
1:	teq	r5, ip
	bne	2f
	orrs	r5, yl, yh, lsl #12
	bne	LSYM(Lml_n)		@ <anything> / NAN -> NAN
	b	LSYM(Lml_z)		@ <anything> / INF -> 0
2:	@ One or both arguments are 0.
	orrs	r4, xl, xh, lsl #1
	bne	LSYM(Lml_i)		@ <non_zero> / 0 -> INF
	orrs	r5, yl, yh, lsl #1
	bne	LSYM(Lml_z)		@ 0 / <non_zero> -> 0
	b	LSYM(Lml_n)		@ 0 / 0 -> NAN

	FUNC_END divdf3

#endif /* L_muldivdf3 */

#ifdef L_cmpdf2

ARM_FUNC_START gtdf2
ARM_FUNC_ALIAS gedf2 gtdf2
	mov	ip, #-1
	b	1f

ARM_FUNC_START ltdf2
ARM_FUNC_ALIAS ledf2 ltdf2
	mov	ip, #1
	b	1f

ARM_FUNC_START cmpdf2
ARM_FUNC_ALIAS nedf2 cmpdf2
ARM_FUNC_ALIAS eqdf2 cmpdf2
	mov	ip, #1			@ how should we specify unordered here?

1:	stmfd	sp!, {r4, r5, lr}

	@ Trap any INF/NAN first.
	mov	lr, #0x7f000000
	orr	lr, lr, #0x00f00000
	and	r4, xh, lr
	and	r5, yh, lr
	teq	r4, lr
	teqne	r5, lr
	beq	3f

	@ Test for equality.
	@ Note that 0.0 is equal to -0.0.
2:	orrs	ip, xl, xh, lsl #1	@ if x == 0.0 or -0.0
	orreqs	ip, yl, yh, lsl #1	@ and y == 0.0 or -0.0
	teqne	xh, yh			@ or xh == yh
	teqeq	xl, yl			@ and xl == yl
	moveq	r0, #0			@ then equal.
	RETLDM	"r4, r5" eq

	@ Check for sign difference.
	teq	xh, yh
	movmi	r0, xh, asr #31
	orrmi	r0, r0, #1
	RETLDM	"r4, r5" mi

	@ Compare exponents.
	cmp	r4, r5

	@ Compare mantissa if exponents are equal.
	moveq	xh, xh, lsl #12
	cmpeq	xh, yh, lsl #12
	cmpeq	xl, yl
	movcs	r0, yh, asr #31
	mvncc	r0, yh, asr #31
	orr	r0, r0, #1
	RETLDM	"r4, r5"

	@ Look for a NAN.
3:	teq	r4, lr
	bne	4f
	orrs	xl, xl, xh, lsl #12
	bne	5f			@ x is NAN
4:	teq	r5, lr
	bne	2b
	orrs	yl, yl, yh, lsl #12
	beq	2b			@ y is not NAN
5:	mov	r0, ip			@ return unordered code from ip
	RETLDM	"r4, r5"

	FUNC_END gedf2
	FUNC_END gtdf2
	FUNC_END ledf2
	FUNC_END ltdf2
	FUNC_END nedf2
	FUNC_END eqdf2
	FUNC_END cmpdf2

#endif /* L_cmpdf2 */

#ifdef L_unorddf2

ARM_FUNC_START unorddf2
	str	lr, [sp, #-4]!
	mov	ip, #0x7f000000
	orr	ip, ip, #0x00f00000
	and	lr, xh, ip
	teq	lr, ip
	bne	1f
	orrs	xl, xl, xh, lsl #12
	bne	3f			@ x is NAN
1:	and	lr, yh, ip
	teq	lr, ip
	bne	2f
	orrs	yl, yl, yh, lsl #12
	bne	3f			@ y is NAN
2:	mov	r0, #0			@ arguments are ordered.
	RETLDM

3:	mov	r0, #1			@ arguments are unordered.
	RETLDM

	FUNC_END unorddf2

#endif /* L_unorddf2 */

#ifdef L_fixdfsi

ARM_FUNC_START fixdfsi
	orrs	ip, xl, xh, lsl #1
	beq	1f			@ value is 0.

	mov	r3, r3, rrx		@ preserve C flag (the actual sign)

	@ check exponent range.
	mov	ip, #0x7f000000
	orr	ip, ip, #0x00f00000
	and	r2, xh, ip
	teq	r2, ip
	beq	2f			@ value is INF or NAN
	bic	ip, ip, #0x40000000
	cmp	r2, ip
	bcc	1f			@ value is too small
	add	ip, ip, #(31 << 20)
	cmp	r2, ip
	bcs	3f			@ value is too large

	rsb	r2, r2, ip
	mov	ip, xh, lsl #11
	orr	ip, ip, #0x80000000
	orr	ip, ip, xl, lsr #21
	mov	r2, r2, lsr #20
	tst	r3, #0x80000000		@ the sign bit
	mov	r0, ip, lsr r2
	rsbne	r0, r0, #0
	RET

1:	mov	r0, #0
	RET

2:	orrs	xl, xl, xh, lsl #12
	bne	4f			@ r0 is NAN.
3:	ands	r0, r3, #0x80000000	@ the sign bit
	moveq	r0, #0x7fffffff		@ maximum signed positive si
	RET

4:	mov	r0, #0			@ How should we convert NAN?
	RET

	FUNC_END fixdfsi

#endif /* L_fixdfsi */

#ifdef L_fixunsdfsi

ARM_FUNC_START fixunsdfsi
	orrs	ip, xl, xh, lsl #1
	movcss	r0, #0			@ value is negative
	RETc(eq)			@ or 0 (xl, xh overlap r0)

	@ check exponent range.
	mov	ip, #0x7f000000
	orr	ip, ip, #0x00f00000
	and	r2, xh, ip
	teq	r2, ip
	beq	2f			@ value is INF or NAN
	bic	ip, ip, #0x40000000
	cmp	r2, ip
	bcc	1f			@ value is too small
	add	ip, ip, #(31 << 20)
	cmp	r2, ip
	bhi	3f			@ value is too large

	rsb	r2, r2, ip
	mov	ip, xh, lsl #11
	orr	ip, ip, #0x80000000
	orr	ip, ip, xl, lsr #21
	mov	r2, r2, lsr #20
	mov	r0, ip, lsr r2
	RET

1:	mov	r0, #0
	RET

2:	orrs	xl, xl, xh, lsl #12
	bne	4f			@ value is NAN.
3:	mov	r0, #0xffffffff		@ maximum unsigned si
	RET

4:	mov	r0, #0			@ How should we convert NAN?
	RET

	FUNC_END fixunsdfsi

#endif /* L_fixunsdfsi */

#ifdef L_truncdfsf2

ARM_FUNC_START truncdfsf2
	orrs	r2, xl, xh, lsl #1
	moveq	r0, r2, rrx
	RETc(eq)			@ value is 0.0 or -0.0
	
	@ check exponent range.
	mov	ip, #0x7f000000
	orr	ip, ip, #0x00f00000
	and	r2, ip, xh
	teq	r2, ip
	beq	2f			@ value is INF or NAN
	bic	xh, xh, ip
	cmp	r2, #(0x380 << 20)
	bls	4f			@ value is too small

	@ shift and round mantissa
1:	movs	r3, xl, lsr #29
	adc	r3, r3, xh, lsl #3

	@ if halfway between two numbers, round towards LSB = 0.
	mov	xl, xl, lsl #3
	teq	xl, #0x80000000
	biceq	r3, r3, #1

	@ rounding might have created an extra MSB.  If so adjust exponent.
	tst	r3, #0x00800000
	addne	r2, r2, #(1 << 20)
	bicne	r3, r3, #0x00800000

	@ check exponent for overflow
	mov	ip, #(0x400 << 20)
	orr	ip, ip, #(0x07f << 20)
	cmp	r2, ip
	bcs	3f			@ overflow

	@ adjust exponent, merge with sign bit and mantissa.
	movs	xh, xh, lsl #1
	mov	r2, r2, lsl #4
	orr	r0, r3, r2, rrx
	eor	r0, r0, #0x40000000
	RET

2:	@ chech for NAN
	orrs	xl, xl, xh, lsl #12
	movne	r0, #0x7f000000
	orrne	r0, r0, #0x00c00000
	RETc(ne)			@ return NAN

3:	@ return INF with sign
	and	r0, xh, #0x80000000
	orr	r0, r0, #0x7f000000
	orr	r0, r0, #0x00800000
	RET

4:	@ check if denormalized value is possible
	subs	r2, r2, #((0x380 - 24) << 20)
	andle	r0, xh, #0x80000000	@ too small, return signed 0.
	RETc(le)
	
	@ denormalize value so we can resume with the code above afterwards.
	orr	xh, xh, #0x00100000
	mov	r2, r2, lsr #20
	rsb	r2, r2, #25
	cmp	r2, #20
	bgt	6f

	rsb	ip, r2, #32
	mov	r3, xl, lsl ip
	mov	xl, xl, lsr r2
	orr	xl, xl, xh, lsl ip
	movs	xh, xh, lsl #1
	mov	xh, xh, lsr r2
	mov	xh, xh, rrx
5:	teq	r3, #0			@ fold r3 bits into the LSB
	orrne	xl, xl, #1		@ for rounding considerations. 
	mov	r2, #(0x380 << 20)	@ equivalent to the 0 float exponent
	b	1b

6:	rsb	r2, r2, #(12 + 20)
	rsb	ip, r2, #32
	mov	r3, xl, lsl r2
	mov	xl, xl, lsr ip
	orr	xl, xl, xh, lsl r2
	and	xh, xh, #0x80000000
	b	5b

	FUNC_END truncdfsf2

#endif /* L_truncdfsf2 */
