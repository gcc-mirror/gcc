;; libgcc1 routines for the Hitachi h8/300 cpu.
;; Contributed by Steve Chamberlain.
;; sac@cygnus.com

/* Copyright (C) 1994 Free Software Foundation, Inc.

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

/* Assembler register definitions.  */

#define A0 r0
#define A0L r0l
#define A0H r0h

#define A1 r1
#define A1L r1l
#define A1H r1h

#define A2 r2
#define A2L r2l
#define A2H r2h

#define A3 r3
#define A3L r3l
#define A3H r3h

#define S0 r4
#define S0L r4l
#define S0H r4h

#define S1 r5
#define S1L r5l
#define S1H r5h

#define S2 r6
#define S2L r6l
#define S2H r6h

#ifdef __H8300__
#define MOVP	mov.w	/* pointers are 16 bits */
#define ADDP	add.w
#define CMPP	cmp.w
#define PUSHP	push
#define POPP	pop

#define A0P	r0
#define A1P	r1
#define A2P	r2
#define A3P	r3
#define S0P	r4
#define S1P	r5
#define S2P	r6
#endif

#ifdef __H8300H__
#define MOVP	mov.l	/* pointers are 32 bits */
#define ADDP	add.l
#define CMPP	cmp.l
#define PUSHP	push.l
#define POPP	pop.l

#define A0P	er0
#define A1P	er1
#define A2P	er2
#define A3P	er3
#define S0P	er4
#define S1P	er5
#define S2P	er6

#define A0E	e0
#define A1E	e1
#define A2E	e2
#define A3E	e3
#endif

#ifdef L_cmpsi2
#ifdef __H8300__
	.section .text
	.align 2
	.global ___cmpsi2
___cmpsi2:
	cmp.w	A2,A0
	bne	.L2
	cmp.w	A3,A1
	bne	.L2
	mov.w	#1,A0
	rts
.L2:
	cmp.w	A0,A2
	bgt	.L4
	bne	.L3
	cmp.w	A1,A3
	bls	.L3
.L4:
	sub.w	A0,A0
	rts
.L3:
	mov.w	#2,A0
.L5:
	rts
	.end
#endif
#endif /* L_cmpsi2 */

#ifdef L_ucmpsi2
#ifdef __H8300__
	.section .text
	.align 2
	.global ___ucmpsi2
___ucmpsi2:
	cmp.w	A2,A0
	bne	.L2
	cmp.w	A3,A1
	bne	.L2
	mov.w	#1,A0
	rts
.L2:
	cmp.w	A0,A2
	bhi	.L4
	bne	.L3
	cmp.w	A1,A3
	bls	.L3
.L4:
	sub.w	A0,A0
	rts
.L3:
	mov.w	#2,A0
.L5:
	rts
	.end
#endif
#endif /* L_ucmpsi2 */

#ifdef L_divhi3

;; HImode divides for the H8/300.
;; We bunch all of this into one object file since there are several
;; "supporting routines".

; general purpose normalize routine
; 
; divisor in A0
; dividend in A1
; turns both into +ve numbers, and leaves what the answer sign
; should be in A2L

#ifdef __H8300__
	.section .text
	.align 2
divnorm:
	mov.b	#0x0,A2L
	or	A0H,A0H		; is divisor > 0
	bge	_lab1			
	not	A0H		; no - then make it +ve
	not	A0L
	adds	#1,A0			
	xor	#0x1,A2L	; and remember that in A2L
_lab1:	or	A1H,A1H	; look at dividend
	bge	_lab2		
	not	A1H		; it is -ve, make it positive
	not	A1L
	adds	#1,A1
	xor	#0x1,A2L; and toggle sign of result
_lab2:	rts

; A0=A0/A1 signed

	.global	___divhi3
___divhi3:
	bsr	divnorm
	bsr	___udivhi3
negans:	or	A2L,A2L	; should answer be negative ?
	beq	_lab4
	not	A0H	; yes, so make it so
	not	A0L
	adds	#1,A0
_lab4:	rts	

; A0=A0%A1 signed

	.global	___modhi3
___modhi3:
	bsr	divnorm
	bsr	___udivhi3
	mov	A3,A0
	bra	negans

; A0=A0%A1 unsigned

	.global	___umodhi3
___umodhi3:
	bsr	___udivhi3
	mov	A3,A0
	rts

; A0=A0/A1 unsigned
; A3=A0%A1 unsigned
; A2H trashed
; D high 8 bits of denom
; d low 8 bits of denom
; N high 8 bits of num
; n low 8 bits of num
; M high 8 bits of mod
; m low 8 bits of mod
; Q high 8 bits of quot
; q low 8 bits of quot
; P preserve

; The h8 only has a 16/8 bit divide, so we look at the incoming and
; see how to partition up the expression.

	.global	___udivhi3
___udivhi3:
				; A0 A1 A2 A3 
				; Nn Dd       P
	sub.w	A3,A3		; Nn Dd xP 00 
	or	A1H,A1H		 
	bne	divlongway
	or	A0H,A0H		
	beq	_lab6		

; we know that D == 0 and N is != 0
	mov.b	A0H,A3L		; Nn Dd xP 0N
	divxu	A1L,A3		;          MQ
	mov.b	A3L,A0H	 	; Q
; dealt with N, do n
_lab6:	mov.b	A0L,A3L		;           n
	divxu	A1L,A3		;          mq
	mov.b	A3L,A0L		; Qq
	mov.b	A3H,A3L         ;           m
	mov.b	#0x0,A3H	; Qq       0m
	rts	

; D != 0 - which means the denominator is
;          loop around to get the result.

divlongway:
	mov.b	A0H,A3L		; Nn Dd xP 0N
	mov.b	#0x0,A0H	; high byte of answer has to be zero
	mov.b	#0x8,A2H	;       8
div8:	add.b	A0L,A0L		; n*=2
	rotxl	A3L		; Make remainder bigger
	rotxl	A3H		
	sub.w	A1,A3		; Q-=N
	bhs	setbit		; set a bit ?
	add.w	A1,A3		;  no : too far , Q+=N

	dec	A2H		
	bne	div8		; next bit	
	rts	

setbit:	inc	A0L		; do insert bit
	dec	A2H		
	bne	div8		; next bit	
	rts	

#endif /* __H8300__ */
#endif /* L_divhi3 */

#ifdef L_divsi3

;; 4 byte integer divides for the H8/300.
;;
;; We have one routine which does all the work and lots of 
;; little ones which prepare the args and massage the sign.
;; We bunch all of this into one object file since there are several
;; "supporting routines".

#ifdef __H8300H__
	.h8300h
#endif

	.section .text
	.align 2

; Put abs SIs into r0/r1 and r2/r3, and leave a 1 in r6l with sign of rest.
; This function is here to keep branch displacements small.

#ifdef __H8300__

divnorm:
	mov.b	#0,S2L		; keep the sign in S2
	mov.b	A0H,A0H		; is the numerator -ve
	bge	postive

	; negate arg
	not	A0H
	not	A1H
	not	A0L
	not	A1L

	add	#1,A1L
	addx	#0,A1H
	addx	#0,A0H
	addx	#0,A0L

	mov.b	#1,S2L		; the sign will be -ve
postive:
	mov.b	A2H,A2H		; is the denominator -ve
	bge	postive2
	not	A2L		
	not	A2H
	not	A3L
	not	A3H
	add.b	#1,A3L	
	addx	#0,A3H
	addx	#0,A2L
	addx	#0,A2H
	xor	#1,S2L		; toggle result sign
postive2:
	rts

#else /* __H8300H__ */

divnorm:
	mov.b	#0,S2L		; keep the sign in S2
	mov.l	A0P,A0P		; is the numerator -ve
	bge	postive

	neg.l	A0P		; negate arg
	mov.b	#1,S2L		; the sign will be -ve

postive:
	mov.l	A1P,A1P		; is the denominator -ve
	bge	postive2

	neg.l	A1P		; negate arg
	xor.b	#1,S2L		; toggle result sign

postive2:
	rts

#endif

; numerator in A0/A1
; denominator in A2/A3
	.global	___modsi3
___modsi3:
	PUSHP	S2P		
	PUSHP	S0P
	PUSHP	S1P

	bsr	divnorm
	bsr	divmodsi4
#ifdef __H8300__
	mov	S0,A0
	mov	S1,A1
#else
	mov.l	S0P,A0P
#endif
	bra	exitdiv

	.global	___udivsi3
___udivsi3:
	PUSHP	S2P
	PUSHP	S0P
	PUSHP	S1P
	mov.b	#0,S2L	; keep sign low
	bsr	divmodsi4
	bra	exitdiv

	.global	___umodsi3
___umodsi3:
	PUSHP	S2P
	PUSHP	S0P
	PUSHP	S1P
	mov.b	#0,S2L	; keep sign low
	bsr	divmodsi4
#ifdef __H8300__
	mov	S0,A0
	mov	S1,A1
#else
	mov.l	S0P,A0P
#endif
	bra	exitdiv
	
	.global	___divsi3
___divsi3:
	PUSHP	S2P
	PUSHP	S0P
	PUSHP	S1P
	jsr	divnorm
	jsr	divmodsi4

	; examine what the sign should be
exitdiv:
	POPP	S1P
	POPP	S0P

	or	S2L,S2L
	beq	reti
	
	; should be -ve
#ifdef __H8300__
	not	A0H
	not	A1H
	not	A0L
	not	A1L

	add	#1,A1L
	addx	#0,A1H
	addx	#0,A0H
	addx	#0,A0L
#else /* __H8300H__ */
	neg.l	A0P
#endif

reti:
	POPP	S2P
	rts	

	; takes A0/A1 numerator (A0P for 300h)
	; A2/A3 denominator (A1P for 300h)
	; returns A0/A1 quotient (A0P for 300h)
	; S0/S1 remainder (S0P for 300h)
	; trashes S2

#ifdef __H8300__

divmodsi4:
        sub.w	S0,S0		; zero play area
        mov.w	S0,S1
        mov.b	A2H,S2H
        or	A2L,S2H
        or	A3H,S2H
        bne	DenHighZero
        mov.b	A0H,A0H
        bne	NumByte0Zero
        mov.b	A0L,A0L
        bne	NumByte1Zero
        mov.b	A1H,A1H
        bne	NumByte2Zero
        bra	NumByte3Zero
NumByte0Zero:
	mov.b	A0H,S1L
        divxu	A3L,S1
        mov.b	S1L,A0H
NumByte1Zero:
	mov.b	A0L,S1L
        divxu	A3L,S1
        mov.b	S1L,A0L
NumByte2Zero:
	mov.b	A1H,S1L
        divxu	A3L,S1
        mov.b	S1L,A1H
NumByte3Zero:
	mov.b	A1L,S1L
        divxu	A3L,S1
        mov.b	S1L,A1L

        mov.b	S1H,S1L
        mov.b	#0x0,S1H
        rts	

; have to do the divide by shift and test
DenHighZero:
	mov.b	A0H,S1L
        mov.b	A0L,A0H
        mov.b	A1H,A0L
        mov.b	A1L,A1H

        mov.b	#0,A1L
        mov.b	#24,S2H	; only do 24 iterations

nextbit:
	add.w	A1,A1	; double the answer guess
        rotxl	A0L
        rotxl	A0H

        rotxl	S1L	; double remainder
        rotxl	S1H
        rotxl	S0L
        rotxl	S0H
        sub.w	A3,S1	; does it all fit
        subx	A2L,S0L
        subx	A2H,S0H
        bhs	setone	 

        add.w	A3,S1	; no, restore mistake
        addx	A2L,S0L
        addx	A2H,S0H

        dec	S2H
        bne	nextbit
        rts	
	
setone:
	inc	A1L
        dec	S2H
        bne	nextbit
        rts	

#else /* __H8300H__ */

divmodsi4:
	sub.l	S0P,S0P		; zero play area
	mov.w	A1E,A1E		; denominator top word 0?
	bne	DenHighZero

	; do it the easy way, see page 107 in manual
	mov.w	A0E,A2
	extu.l	A2P
	divxu.w	A1,A2P
	mov.w	A2E,A0E
	divxu.w	A1,A0P
	mov.w	A0E,S0
	mov.w	A2,A0E
	extu.l	S0P
	rts

DenHighZero:
	mov.w	A0E,A2
	mov.b	A2H,S0L
	mov.b	A2L,A2H
	mov.b	A0H,A2L
	mov.w	A2,A0E
	mov.b	A0L,A0H
	mov.b	#0,A0L
	mov.b	#24,S2H		; only do 24 iterations

nextbit:
	shll.l	A0P		; double the answer guess
	rotxl.l	S0P		; double remainder
	sub.l	A1P,S0P		; does it all fit?
	bhs	setone

	add.l	A1P,S0P		; no, restore mistake
	dec	S2H
	bne	nextbit
	rts

setone:
	inc	A0L
	dec	S2H
	bne	nextbit
	rts

#endif
#endif /* L_divsi3 */

#ifdef L_mulhi3

;; HImode multiply.
; The h8 only has an 8*8->16 multiply.
; The answer is the same as:
; 
; product = (srca.l * srcb.l) + ((srca.h * srcb.l) + (srcb.h * srca.l)) * 256
; (we can ignore A1.h * A0.h cause that will all off the top)
; A0 in
; A1 in 
; A0 answer

#ifdef __H8300__
	.section .text
	.align 2
	.global	___mulhi3
___mulhi3:
	mov.b	A1L,A2L		; A2l gets srcb.l
	mulxu	A0L,A2		; A2 gets first sub product 

	mov.b	A0H,A3L		; prepare for
	mulxu	A1L,A3		; second sub product

	add.b	A3L,A2H		; sum first two terms

	mov.b	A1H,A3L		; third sub product
	mulxu	A0L,A3		

	add.b	A3L,A2H		; almost there
	mov.w	A2,A0		; that is
	rts

#endif
#endif /* L_mulhi3 */

#ifdef L_mulsi3

;; SImode multiply.
;; 
;; I think that shift and add may be sufficient for this.  Using the
;; supplied 8x8->16 would need 10 ops of 14 cycles each + overhead.  This way
;; the inner loop uses maybe 20 cycles + overhead, but terminates
;; quickly on small args.
;;
;; A0/A1 src_a
;; A2/A3 src_b
;;
;;  while (a) 
;;    {
;;      if (a & 1)
;;        r += b;
;;      a >>= 1;
;;      b <<= 1;
;;    }

	.section .text
	.align 2

#ifdef __H8300__

	.global	___mulsi3
___mulsi3:
	PUSHP	S0P
	PUSHP	S1P
	PUSHP	S2P
	
	sub.w	S0,S0
	sub.w	S1,S1
	
	; while (a)
_top:	mov.w	A0,A0
	bne	_more
	mov.w	A1,A1
	beq	_done
_more:	; if (a & 1)
	bld	#0,A1L
	bcc	_nobit
	; r += b
	add.w	A3,S1
	addx	A2L,S0L
	addx	A2H,S0H
_nobit:
	; a >>= 1
	shlr	A0H
	rotxr	A0L
	rotxr	A1H
	rotxr	A1L
	
	; b <<= 1
	add.w	A3,A3
	addx	A2L,A2L
	addx	A2H,A2H
	bra 	_top

_done:
	mov.w	S0,A0	
	mov.w	S1,A1
	POPP	S2P
	POPP	S1P
	POPP	S0P
	rts

#else /* __H8300H__ */

	.h8300h

	.global	___mulsi3
___mulsi3:
	sub.l	A2P,A2P

	; while (a)
_top:	mov.l	A0P,A0P
	beq	_done

	; if (a & 1)
	bld	#0,A0L
	bcc	_nobit

	; r += b
	add.l	A1P,A2P

_nobit:
	; a >>= 1
	shlr.l	A0P

	; b <<= 1
	shll.l	A1P
	bra	_top

_done:
	mov.l	A2P,A0P
	rts

#endif
#endif /* L_mulsi3 */
