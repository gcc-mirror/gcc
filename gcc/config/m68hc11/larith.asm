/* libgcc routines for M68HC11 & M68HC12.
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it
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

	.file "larith.asm"

	.sect .text
	

#define REG(NAME)			\
NAME:	.word 0;			\
	.type NAME,@object ;		\
	.size NAME,2

#ifdef L_regs_min
/* Pseudo hard registers used by gcc.
   They must be located in page0. 
   They will normally appear at the end of .page0 section.  */
#ifdef mc68hc12
	.sect .bss
#else
	.sect .page0
#endif
	.globl _.tmp
	.globl _.z,_.xy
REG(_.tmp)
REG(_.z)
REG(_.xy)

#endif

#ifdef L_regs_frame
#ifdef mc68hc12
	.sect .bss
#else
	.sect .page0
#endif
	.globl _.frame
REG(_.frame)
#endif

#ifdef L_regs_d1_2
#ifdef mc68hc12
	.sect .bss
#else
	.sect .page0
#endif
	.globl _.d1,_.d2
REG(_.d1)
REG(_.d2)
#endif

#ifdef L_regs_d3_4
#ifdef mc68hc12
	.sect .bss
#else
	.sect .page0
#endif
	.globl _.d3,_.d4
REG(_.d3)
REG(_.d4)
#endif

#ifdef L_regs_d5_6
#ifdef mc68hc12
	.sect .bss
#else
	.sect .page0
#endif
	.globl _.d5,_.d6
REG(_.d5)
REG(_.d6)
#endif

#ifdef L_regs_d7_8
#ifdef mc68hc12
	.sect .bss
#else
	.sect .page0
#endif
	.globl _.d7,_.d8
REG(_.d7)
REG(_.d8)
#endif

#ifdef L_regs_d9_16
/* Pseudo hard registers used by gcc.
   They must be located in page0. 
   They will normally appear at the end of .page0 section.  */
	.sect .page0
	.globl _.d9,_.d10,_.d11,_.d12,_.d13,_.d14
	.globl _.d15,_.d16
REG(_.d9)
REG(_.d10)
REG(_.d11)
REG(_.d12)
REG(_.d13)
REG(_.d14)
REG(_.d15)
REG(_.d16)

#endif

#ifdef L_regs_d17_32
/* Pseudo hard registers used by gcc.
   They must be located in page0. 
   They will normally appear at the end of .page0 section.  */
#ifdef mc68hc12
	.sect .bss
#else
	.sect .page0
#endif
	.globl _.d17,_.d18,_.d19,_.d20,_.d21,_.d22
	.globl _.d23,_.d24,_.d25,_.d26,_.d27,_.d28
	.globl _.d29,_.d30,_.d31,_.d32
REG(_.d17)
REG(_.d18)
REG(_.d19)
REG(_.d20)
REG(_.d21)
REG(_.d22)
REG(_.d23)
REG(_.d24)
REG(_.d25)
REG(_.d26)
REG(_.d27)
REG(_.d28)
REG(_.d29)
REG(_.d30)
REG(_.d31)
REG(_.d32)
#endif

#ifdef L_premain
;;
;; Specific initialization for 68hc11 before the main.
;; Nothing special for a generic routine; Just enable interrupts.
;;
	.sect .text
	.globl __premain
__premain:
	clra
	tap	; Clear both I and X.
	rts
#endif

#ifdef L__exit
;;
;; Exit operation.  Just loop forever and wait for interrupts.
;; (no other place to go)
;; This operation is split in several pieces collected together by
;; the linker script.  This allows to support destructors at the
;; exit stage while not impacting program sizes when there is no
;; destructors.
;;
;; _exit:
;;    *(.fini0)		/* Beginning of finish code (_exit symbol).  */
;;    *(.fini1)		/* Place holder for applications.  */
;;    *(.fini2)		/* C++ destructors.  */
;;    *(.fini3)		/* Place holder for applications.  */
;;    *(.fini4)		/* Runtime exit.  */
;;
	.sect .fini0,"ax",@progbits
	.globl _exit
	.globl exit
	.weak  exit
exit:
_exit:

	.sect .fini4,"ax",@progbits
fatal:
	cli
	wai
	bra fatal
#endif

#ifdef L_abort
;;
;; Abort operation.  This is defined for the GCC testsuite.
;;
	.sect .text
	.globl abort
abort:
	ldd	#255		; 
#ifdef mc68hc12
	trap	#0x30
#else
	.byte 0xCD		; Generate an illegal instruction trap
	.byte 0x03		; The simulator catches this and stops.
#endif
	jmp _exit
#endif
	
#ifdef L_cleanup
;;
;; Cleanup operation used by exit().
;;
	.sect .text
	.globl _cleanup
_cleanup:
	rts
#endif

;-----------------------------------------
; required gcclib code
;-----------------------------------------
#ifdef L_memcpy
	.sect .text
	.weak memcpy
	.globl memcpy
	.globl __memcpy
;;;
;;; void* memcpy(void*, const void*, size_t)
;;; 
;;; D    = dst	Pmode
;;; 2,sp = src	Pmode
;;; 4,sp = size	HImode (size_t)
;;; 
__memcpy:
memcpy:
#ifdef mc68hc12
	ldx	2,sp
	ldy	4,sp
	pshd
	xgdy
	lsrd
	bcc	Start
	movb	1,x+,1,y+
Start:
	beq	Done
Loop:
	movw	2,x+,2,y+
	dbne	d,Loop
Done:
	puld
	rts
#else
	xgdy
	tsx
	ldd	4,x
	ldx	2,x		; SRC = X, DST = Y
	cpd	#0
	beq	End
	pshy
	inca			; Correction for the deca below
L0:
	psha			; Save high-counter part
L1:
	ldaa	0,x		; Copy up to 256 bytes
	staa	0,y
	inx
	iny
	decb
	bne	L1
	pula
	deca
	bne	L0
	puly			; Restore Y to return the DST
End:
	xgdy
	rts
#endif
#endif

#ifdef L_memset
	.sect .text
	.globl memset
	.globl __memset
;;;
;;; void* memset(void*, int value, size_t)
;;; 
#ifndef __HAVE_SHORT_INT__
;;; D    = dst	Pmode
;;; 2,sp = src	SImode
;;; 6,sp = size	HImode (size_t)
	val  = 5
	size = 6
#else
;;; D    = dst	Pmode
;;; 2,sp = src	SImode
;;; 6,sp = size	HImode (size_t)
	val  = 3
	size = 4
#endif
__memset:
memset:
#ifdef mc68hc12
	xgdx
	ldab	val,sp
	ldy	size,sp
	pshx
	beq	End
Loop:
	stab	1,x+
	dbne	y,Loop
End:
	puld
	rts
#else
	xgdx
	tsy
	ldab	val,y
	ldy	size,y		; DST = X, CNT = Y
	beq	End
	pshx
L0:
	stab	0,x		; Fill up to 256 bytes
	inx
	dey
	bne	L0
	pulx			; Restore X to return the DST
End:
	xgdx
	rts
#endif
#endif
		
#ifdef L_adddi3
	.sect .text
	.globl ___adddi3

___adddi3:
	tsx
	xgdy
	ldd	8,x		; Add LSB
	addd	16,x
	std	6,y		; Save (carry preserved)

	ldd	6,x
	adcb	15,x
	adca	14,x
	std	4,y

	ldd	4,x
	adcb	13,x
	adca	12,x
	std	2,y
	
	ldd	2,x
	adcb	11,x		; Add MSB
	adca	10,x
	std	0,y

	xgdy
	rts
#endif

#ifdef L_subdi3
	.sect .text
	.globl ___subdi3

___subdi3:
	tsx
	xgdy
	ldd	8,x		; Subtract LSB
	subd	16,x
	std	6,y		; Save, borrow preserved

	ldd	6,x
	sbcb	15,x
	sbca	14,x
	std	4,y

	ldd	4,x
	sbcb	13,x
	sbca	12,x
	std	2,y
	
	ldd	2,x		; Subtract MSB
	sbcb	11,x
	sbca	10,x
	std	0,y

	xgdy			;
	rts
#endif
	
#ifdef L_notdi2
	.sect .text
	.globl ___notdi2

___notdi2:
	tsy
	xgdx
	ldd	8,y
	coma
	comb
	std	6,x
	
	ldd	6,y
	coma
	comb
	std	4,x

	ldd	4,y
	coma
	comb
	std	2,x

	ldd	2,y
	coma
	comb
	std	0,x
	xgdx
	rts
#endif
	
#ifdef L_negsi2
	.sect .text
	.globl ___negsi2

___negsi2:
	comb
	coma
	xgdx
	comb
	coma
	inx
	xgdx
	bne	done
	inx
done:
	rts
#endif

#ifdef L_one_cmplsi2
	.sect .text
	.globl ___one_cmplsi2

___one_cmplsi2:
	comb
	coma
	xgdx
	comb
	coma
	xgdx
	rts
#endif
	
#ifdef L_ashlsi3
	.sect .text
	.globl ___ashlsi3

___ashlsi3:
	xgdy
	clra
	andb	#0x1f
	xgdy
	beq	Return
Loop:
	lsld
	xgdx
	rolb
	rola
	xgdx
	dey
	bne	Loop
Return:
	rts
#endif

#ifdef L_ashrsi3
	.sect .text
	.globl ___ashrsi3

___ashrsi3:
	xgdy
	clra
	andb	#0x1f
	xgdy
	beq	Return
Loop:
	xgdx
	asra
	rorb
	xgdx
	rora
	rorb
	dey
	bne	Loop
Return:
	rts
#endif

#ifdef L_lshrsi3
	.sect .text
	.globl ___lshrsi3

___lshrsi3:
	xgdy
	clra
	andb	#0x1f
	xgdy
	beq	Return
Loop:
	xgdx
	lsrd
	xgdx
	rora
	rorb
	dey
	bne	Loop
Return:
	rts
#endif

#ifdef L_lshrhi3
	.sect .text
	.globl ___lshrhi3

___lshrhi3:
	cpx	#16
	bge	Return_zero
	cpx	#0
	beq	Return
Loop:
	lsrd
	dex
	bne	Loop
Return:
	rts
Return_zero:
	clra
	clrb
	rts
#endif
	
#ifdef L_lshlhi3
	.sect .text
	.globl ___lshlhi3

___lshlhi3:
	cpx	#16
	bge	Return_zero
	cpx	#0
	beq	Return
Loop:
	lsld
	dex
	bne	Loop
Return:
	rts
Return_zero:
	clra
	clrb
	rts
#endif

#ifdef L_ashrhi3
	.sect .text
	.globl ___ashrhi3

___ashrhi3:
	cpx	#16
	bge	Return_minus_1_or_zero
	cpx	#0
	beq	Return
Loop:
	asra
	rorb
	dex
	bne	Loop
Return:
	rts
Return_minus_1_or_zero:
	clrb
	tsta
	bpl	Return_zero
	comb
Return_zero:
	tba
	rts
#endif
	
#ifdef L_ashrqi3
	.sect .text
	.globl ___ashrqi3

___ashrqi3:
	cmpa	#8
	bge	Return_minus_1_or_zero
	tsta
	beq	Return
Loop:
	asrb
	deca
	bne	Loop
Return:
	rts
Return_minus_1_or_zero:
	clrb
	tstb
	bpl	Return_zero
	coma
Return_zero:
	tab
	rts
#endif

#ifdef L_lshlqi3
	.sect .text
	.globl ___lshlqi3

___lshlqi3:
	cmpa	#8
	bge	Return_zero
	tsta
	beq	Return
Loop:
	lslb
	deca
	bne	Loop
Return:
	rts
Return_zero:
	clrb
	rts
#endif

#ifdef L_divmodhi4
#ifndef mc68hc12
/* 68HC12 signed divisions are generated inline (idivs).  */

	.sect .text
	.globl __divmodhi4

;
;; D = numerator
;; X = denominator
;;
;; Result:	D = D / X
;;		X = D % X
;; 
__divmodhi4:
	tsta
	bpl	Numerator_pos
	comb			; D = -D <=> D = (~D) + 1
	coma
	xgdx
	inx
	tsta
	bpl	Numerator_neg_denominator_pos
Numerator_neg_denominator_neg:
	comb			; X = -X
	coma
	addd	#1
	xgdx
	idiv
	coma
	comb
	xgdx			; Remainder <= 0 and result >= 0
	inx
	rts

Numerator_pos_denominator_pos:
	xgdx
	idiv
	xgdx			; Both values are >= 0
	rts
	
Numerator_pos:
	xgdx
	tsta
	bpl	Numerator_pos_denominator_pos
Numerator_pos_denominator_neg:
	coma			; X = -X
	comb
	xgdx
	inx
	idiv
	xgdx			; Remainder >= 0 but result <= 0
	coma
	comb
	addd	#1
	rts
	
Numerator_neg_denominator_pos:
	xgdx
	idiv
	coma			; One value is > 0 and the other < 0
	comb			; Change the sign of result and remainder
	xgdx
	inx
	coma
	comb
	addd	#1
	rts
#endif /* !mc68hc12 */
#endif

#ifdef L_mulqi3
       .sect .text
       .globl __mulqi3

;
; short __mulqi3(signed char a, signed char b);
;
;	signed char a	-> register A
;	signed char b	-> register B
;
; returns the signed result of A * B in register D.
;
__mulqi3:
	tsta
	bmi	A_neg
	tstb
	bmi	B_neg
	mul
	rts
B_neg:
	negb
	bra	A_or_B_neg
A_neg:
	nega
	tstb
	bmi	AB_neg
A_or_B_neg:
	mul
	coma
	comb
	addd	#1
	rts
AB_neg:
	negb
	mul
	rts
#endif
	
#ifdef L_mulhi3
	.sect .text
	.globl ___mulhi3

;
;
;  unsigned short ___mulhi3(unsigned short a, unsigned short b)
;
;	a = register D
;	b = register X
;
___mulhi3:
#ifdef mc68hc12
	pshx			; Preserve X
	exg	x,y
	emul
	exg	x,y
	pulx
	rts
#else
#ifdef NO_TMP
	;
	; 16 bit multiplication without temp memory location.
	; (smaller but slower)
	;
	pshx			; (4)
	ins			; (3)
	pshb			; (3)
	psha			; (3)
	pshx			; (4)
	pula			; (4)
	pulx			; (5)
	mul			; (10) B.high * A.low
	xgdx			; (3)
	mul			; (10) B.low * A.high
	abx			; (3)
	pula			; (4)
	pulb			; (4)
	mul			; (10) B.low * A.low
	pshx			; (4) 
	tsx			; (3)
	adda	1,x		; (4)
	pulx			; (5)
	rts			; (5) 20 bytes
				; ---
				; 91 cycles
#else
	stx	_.tmp		; (4/5)
	pshb			; (3)
	ldab	_.tmp+1		; (3/4)
	mul			; (10) B.high * A.low
	xgdx			; (3)
	pulb			; (4)
	stab	_.tmp		; (3/4)
	mul			; (10) B.low * A.high
	abx			; (3)
	ldd	_.tmp		; (4/5)
	mul			; (10) B.low * A.low
	stx	_.tmp		; (4) 
	adda	_.tmp+1		; (4/5)
	rts			; (5) 20/26 bytes
				; ---
				; 70/76 cycles

#ifdef OLD_MUL
	stx	*_.tmp		; (4)
	pshb			; (3)
	ldab	*_.tmp+1	; (3)
	mul			; (10) A.high * B.low
	ldaa	*_.tmp		; (3)
	stab	*_.tmp		; (3)
	pulb			; (4)
	pshb			; (4)
	mul			; (10) A.low * B.high
	addb	*_.tmp		; (4)
	stab	*_.tmp		; (3)
	ldaa	*_.tmp+1	; (3)
	pulb			; (4)
	mul			; (10) A.low * B.low
	adda	*_.tmp		; (4)
	rts			; (5) 24/32 bytes
				; 77/85 cycles
#endif
#endif
#endif
#endif

#ifdef L_mulhi32
	.sect .text
	.globl __mulhi32

;
;
;  unsigned long __mulhi32(unsigned short a, unsigned short b)
;
;	a = register D
;	b = value on stack
;
;	+---------------+
;       |  B low	| <- 7,x
;	+---------------+
;       |  B high	| <- 6,x
;	+---------------+
;       |  PC low	|  
;	+---------------+
;       |  PC high	|  
;	+---------------+
;	|  Tmp low	|
;	+---------------+
;	|  Tmp high     |
;	+---------------+
;	|  A low	|
;	+---------------+
;	|  A high	|
;	+---------------+  <- 0,x
;
;
;      <B-low>    5,x
;      <B-high>   4,x
;      <ret>      2,x
;      <A-low>    1,x
;      <A-high>   0,x
;
__mulhi32:
#ifdef mc68hc12
	ldy	2,sp
	emul
	exg	x,y
	rts
#else
	pshx			; Room for temp value
	pshb
	psha
	tsx
	ldab	6,x
	mul
	xgdy			; A.high * B.high
	ldab	7,x
	pula
	mul			; A.high * B.low
	std	2,x
	ldaa	1,x
	ldab	6,x
	mul			; A.low * B.high
	addd	2,x
	stab	2,x
	tab
	aby
	bcc	N
	ldab	#0xff
	aby
	iny
N:
	ldab	7,x
	pula
	mul			; A.low * B.low
	adda	2,x
	pulx			; Drop temp location
	pshy			; Put high part in X
	pulx
	bcc	Ret
	inx
Ret:
	rts
#endif	
#endif

#ifdef L_mulsi3
	.sect .text
	.globl __mulsi3

;
;      <B-low>    8,y
;      <B-high>   6,y
;      <ret>      4,y
;	<tmp>	  2,y
;      <A-low>    0,y
;
; D,X   -> A
; Stack -> B
;
; The result is:
;
;	(((A.low * B.high) + (A.high * B.low)) << 16) + (A.low * B.low)
;
;
;

__mulsi3:
#ifdef mc68hc12
	pshd				; Save A.low
	ldy	4,sp
	emul				; A.low * B.high
	ldy	6,sp
	exg	x,d
	emul				; A.high * B.low
	leax	d,x
	ldy	6,sp
	puld
	emul				; A.low * B.low
	exg	d,y
	leax	d,x
	exg	d,y
	rts
#else
B_low	=	8
B_high	=	6
A_low	=	0
A_high	=	2
	pshx
	pshb
	psha
	tsy
;
; If B.low is 0, optimize into: (A.low * B.high) << 16
;
	ldd	B_low,y
	beq	B_low_zero
;
; If A.high is 0, optimize into: (A.low * B.high) << 16 + (A.low * B.low)
;
	cpx	#0
	beq	A_high_zero
	bsr	___mulhi3		; A.high * B.low
;
; If A.low is 0, optimize into: (A.high * B.low) << 16
;
	ldx	A_low,y
	beq	A_low_zero		; X = 0, D = A.high * B.low
	std	2,y
;
; If B.high is 0, we can avoid the (A.low * B.high) << 16 term.
;
	ldd	B_high,y
	beq	B_high_zero
	bsr	___mulhi3		; A.low * B.high
	addd	2,y
	std	2,y
;
; Here, we know that A.low and B.low are not 0.
;
B_high_zero:
	ldd	B_low,y			; A.low is on the stack
	bsr	__mulhi32		; A.low * B.low
	xgdx
	tsy				; Y was clobbered, get it back
	addd	2,y
A_low_zero:				; See A_low_zero_non_optimized below
	xgdx
Return:
	ins
	ins
	ins
	ins
	rts
;
; 
; A_low_zero_non_optimized:
;
; At this step, X = 0 and D = (A.high * B.low)
; Optimize into: (A.high * B.low) << 16
;
;	xgdx
;	clra			; Since X was 0, clearing D is superfuous.
;	clrb
;	bra	Return
; ----------------
; B.low == 0, the result is:	(A.low * B.high) << 16
;
; At this step:
;   D = B.low				= 0 
;   X = A.high				?
;       A.low is at A_low,y		?
;       B.low is at B_low,y		?
;
B_low_zero:
	ldd	A_low,y
	beq	Zero1
	ldx	B_high,y
	beq	Zero2
	bsr	___mulhi3
Zero1:
	xgdx
Zero2:
	clra
	clrb
	bra	Return
; ----------------
; A.high is 0, optimize into: (A.low * B.high) << 16 + (A.low * B.low)
;
; At this step:
;   D = B.low				!= 0 
;   X = A.high				= 0
;       A.low is at A_low,y		?
;       B.low is at B_low,y		?
;
A_high_zero:
	ldd	A_low,y		; A.low
	beq	Zero1
	ldx	B_high,y	; B.high
	beq	A_low_B_low
	bsr	___mulhi3
	std	2,y
	bra	B_high_zero	; Do the (A.low * B.low) and the add.

; ----------------
; A.high and B.high are 0 optimize into: (A.low * B.low)
;
; At this step:
;   D = B.high				= 0 
;   X = A.low				!= 0
;       A.low is at A_low,y		!= 0
;       B.high is at B_high,y		= 0
;
A_low_B_low:
	ldd	B_low,y			; A.low is on the stack
	bsr	__mulhi32
	bra	Return
#endif
#endif

#ifdef L_map_data

	.sect	.install2,"ax",@progbits
	.globl	__map_data_section
	.globl __data_image
#ifdef mc68hc12
	.globl __data_section_size
#endif
__map_data_section:
#ifdef mc68hc12
	ldx	#__data_image
	ldy	#__data_section_start
	ldd	#__data_section_size
	beq	Done
Loop:
	movb	1,x+,1,y+
	dbne	d,Loop
#else
	ldx	#__data_image
	ldy	#__data_section_start
	bra	Start_map
Loop:
	ldaa	0,x
	staa	0,y
	inx
	iny
Start_map:
	cpx	#__data_image_end
	blo	Loop
#endif
Done:

#endif

#ifdef L_init_bss

	.sect	.install2,"ax",@progbits
	.globl	__init_bss_section

__init_bss_section:
	ldd	#__bss_size
	beq	Done
	ldx	#__bss_start
Loop:
#ifdef mc68hc12
	clr	1,x+
	dbne	d,Loop
#else
	clr	0,x
	inx
	subd	#1
	bne	Loop
#endif
Done:

#endif

#ifdef L_ctor

; End of constructor table
	.sect	.install3,"ax",@progbits
	.globl	__do_global_ctors

__do_global_ctors:
	; Start from the end - sizeof(void*)
	ldx	#__CTOR_END__-2
ctors_loop:
	cpx	#__CTOR_LIST__
	blo	ctors_done
	pshx
	ldx	0,x
	jsr	0,x
	pulx
	dex
	dex
	bra	ctors_loop
ctors_done:

#endif

#ifdef L_dtor

	.sect	.fini3,"ax",@progbits
	.globl	__do_global_dtors

;;
;; This piece of code is inserted in the _exit() code by the linker.
;;
__do_global_dtors:
	pshb	; Save exit code
	psha
	ldx	#__DTOR_LIST__
dtors_loop:
	cpx	#__DTOR_END__
	bhs	dtors_done
	pshx
	ldx	0,x
	jsr	0,x
	pulx
	inx
	inx
	bra	dtors_loop
dtors_done:
	pula	; Restore exit code
	pulb

#endif

;-----------------------------------------
; end required gcclib code
;-----------------------------------------
