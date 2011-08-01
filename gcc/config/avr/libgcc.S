/*  -*- Mode: Asm -*-  */
/* Copyright (C) 1998, 1999, 2000, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Denis Chertykov <chertykov@gmail.com>

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#define __zero_reg__ r1
#define __tmp_reg__ r0
#define __SREG__ 0x3f
#define __SP_H__ 0x3e
#define __SP_L__ 0x3d
#define __RAMPZ__ 0x3B

/* Most of the functions here are called directly from avr.md
   patterns, instead of using the standard libcall mechanisms.
   This can make better code because GCC knows exactly which
   of the call-used registers (not all of them) are clobbered.  */

	.section .text.libgcc, "ax", @progbits

	.macro	mov_l  r_dest, r_src
#if defined (__AVR_HAVE_MOVW__)
	movw	\r_dest, \r_src
#else
	mov	\r_dest, \r_src
#endif
	.endm

	.macro	mov_h  r_dest, r_src
#if defined (__AVR_HAVE_MOVW__)
	; empty
#else
	mov	\r_dest, \r_src
#endif
	.endm

#if defined (__AVR_HAVE_JMP_CALL__)
#define XCALL call
#define XJMP  jmp
#else
#define XCALL rcall
#define XJMP  rjmp
#endif

.macro DEFUN name
.global \name
.func \name
\name:
.endm

.macro ENDF name
.size \name, .-\name
.endfunc
.endm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
/* Note: mulqi3, mulhi3 are open-coded on the enhanced core.  */
#if !defined (__AVR_HAVE_MUL__)
/*******************************************************
    Multiplication  8 x 8  without MUL
*******************************************************/
#if defined (L_mulqi3)

#define	r_arg2	r22		/* multiplicand */
#define	r_arg1 	r24		/* multiplier */
#define r_res	__tmp_reg__	/* result */

DEFUN __mulqi3
	clr	r_res		; clear result
__mulqi3_loop:
	sbrc	r_arg1,0
	add	r_res,r_arg2
	add	r_arg2,r_arg2	; shift multiplicand
	breq	__mulqi3_exit	; while multiplicand != 0
	lsr	r_arg1		; 
	brne	__mulqi3_loop	; exit if multiplier = 0
__mulqi3_exit:	
	mov	r_arg1,r_res	; result to return register
	ret
ENDF __mulqi3

#undef r_arg2  
#undef r_arg1  
#undef r_res   
	
#endif 	/* defined (L_mulqi3) */

#if defined (L_mulqihi3)
DEFUN __mulqihi3
	clr	r25
	sbrc	r24, 7
	dec	r25
	clr	r23
	sbrc	r22, 7
	dec	r22
	rjmp	__mulhi3
ENDF __mulqihi3:
#endif /* defined (L_mulqihi3) */

#if defined (L_umulqihi3)
DEFUN __umulqihi3
	clr	r25
	clr	r23
	rjmp	__mulhi3
ENDF __umulqihi3
#endif /* defined (L_umulqihi3) */

/*******************************************************
    Multiplication  16 x 16  without MUL
*******************************************************/
#if defined (L_mulhi3)
#define	r_arg1L	r24		/* multiplier Low */
#define	r_arg1H	r25		/* multiplier High */
#define	r_arg2L	r22		/* multiplicand Low */
#define	r_arg2H	r23		/* multiplicand High */
#define r_resL	__tmp_reg__	/* result Low */
#define r_resH  r21		/* result High */

DEFUN __mulhi3
	clr	r_resH		; clear result
	clr	r_resL		; clear result
__mulhi3_loop:
	sbrs	r_arg1L,0
	rjmp	__mulhi3_skip1
	add	r_resL,r_arg2L	; result + multiplicand
	adc	r_resH,r_arg2H
__mulhi3_skip1:	
	add	r_arg2L,r_arg2L	; shift multiplicand
	adc	r_arg2H,r_arg2H

	cp	r_arg2L,__zero_reg__
	cpc	r_arg2H,__zero_reg__
	breq	__mulhi3_exit	; while multiplicand != 0

	lsr	r_arg1H		; gets LSB of multiplier
	ror	r_arg1L
	sbiw	r_arg1L,0
	brne	__mulhi3_loop	; exit if multiplier = 0
__mulhi3_exit:
	mov	r_arg1H,r_resH	; result to return register
	mov	r_arg1L,r_resL
	ret
ENDF __mulhi3

#undef r_arg1L
#undef r_arg1H
#undef r_arg2L
#undef r_arg2H
#undef r_resL 	
#undef r_resH 

#endif /* defined (L_mulhi3) */

/*******************************************************
    Widening Multiplication  32 = 16 x 16  without MUL
*******************************************************/

#if defined (L_mulhisi3)
DEFUN __mulhisi3
;;; FIXME: This is dead code (noone calls it)
    mov_l   r18, r24
    mov_h   r19, r25
    clr     r24
    sbrc    r23, 7
    dec     r24
    mov     r25, r24
    clr     r20
    sbrc    r19, 7
    dec     r20
    mov     r21, r20
    XJMP    __mulsi3
ENDF __mulhisi3
#endif /* defined (L_mulhisi3) */

#if defined (L_umulhisi3)
DEFUN __umulhisi3
;;; FIXME: This is dead code (noone calls it)
    mov_l   r18, r24
    mov_h   r19, r25
    clr     r24
    clr     r25
    mov_l   r20, r24
    mov_h   r21, r25
    XJMP    __mulsi3
ENDF __umulhisi3
#endif /* defined (L_umulhisi3) */

#if defined (L_mulsi3)
/*******************************************************
    Multiplication  32 x 32  without MUL
*******************************************************/
#define r_arg1L  r22		/* multiplier Low */
#define r_arg1H  r23
#define	r_arg1HL r24
#define	r_arg1HH r25		/* multiplier High */

#define	r_arg2L  r18		/* multiplicand Low */
#define	r_arg2H  r19	
#define	r_arg2HL r20
#define	r_arg2HH r21		/* multiplicand High */
	
#define r_resL	 r26		/* result Low */
#define r_resH   r27
#define r_resHL	 r30
#define r_resHH  r31		/* result High */

DEFUN __mulsi3
	clr	r_resHH		; clear result
	clr	r_resHL		; clear result
	clr	r_resH		; clear result
	clr	r_resL		; clear result
__mulsi3_loop:
	sbrs	r_arg1L,0
	rjmp	__mulsi3_skip1
	add	r_resL,r_arg2L		; result + multiplicand
	adc	r_resH,r_arg2H
	adc	r_resHL,r_arg2HL
	adc	r_resHH,r_arg2HH
__mulsi3_skip1:
	add	r_arg2L,r_arg2L		; shift multiplicand
	adc	r_arg2H,r_arg2H
	adc	r_arg2HL,r_arg2HL
	adc	r_arg2HH,r_arg2HH
	
	lsr	r_arg1HH	; gets LSB of multiplier
	ror	r_arg1HL
	ror	r_arg1H
	ror	r_arg1L
	brne	__mulsi3_loop
	sbiw	r_arg1HL,0
	cpc	r_arg1H,r_arg1L
	brne	__mulsi3_loop		; exit if multiplier = 0
__mulsi3_exit:
	mov_h	r_arg1HH,r_resHH	; result to return register
	mov_l	r_arg1HL,r_resHL
	mov_h	r_arg1H,r_resH
	mov_l	r_arg1L,r_resL
	ret
ENDF __mulsi3

#undef r_arg1L 
#undef r_arg1H 
#undef r_arg1HL
#undef r_arg1HH
             
#undef r_arg2L 
#undef r_arg2H 
#undef r_arg2HL
#undef r_arg2HH
             
#undef r_resL  
#undef r_resH  
#undef r_resHL 
#undef r_resHH 

#endif /* defined (L_mulsi3) */

#endif /* !defined (__AVR_HAVE_MUL__) */
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#if defined (__AVR_HAVE_MUL__)    
#define A0 26
#define B0 18
#define C0 22

#define A1 A0+1

#define B1 B0+1
#define B2 B0+2
#define B3 B0+3

#define C1 C0+1
#define C2 C0+2
#define C3 C0+3

/*******************************************************
    Widening Multiplication  32 = 16 x 16
*******************************************************/
                              
#if defined (L_mulhisi3)
;;; R25:R22 = (signed long) R27:R26 * (signed long) R19:R18
;;; C3:C0   = (signed long) A1:A0   * (signed long) B1:B0
;;; Clobbers: __tmp_reg__
DEFUN __mulhisi3
    XCALL   __umulhisi3
    ;; Sign-extend B
    tst     B1
    brpl    1f
    sub     C2, A0
    sbc     C3, A1
1:  ;; Sign-extend A
    XJMP __usmulhisi3_tail
ENDF __mulhisi3
#endif /* L_mulhisi3 */

#if defined (L_usmulhisi3)
;;; R25:R22 = (signed long) R27:R26 * (unsigned long) R19:R18
;;; C3:C0   = (signed long) A1:A0   * (unsigned long) B1:B0
;;; Clobbers: __tmp_reg__
DEFUN __usmulhisi3
    XCALL   __umulhisi3
    ;; FALLTHRU
ENDF __usmulhisi3

DEFUN __usmulhisi3_tail
    ;; Sign-extend A
    sbrs    A1, 7
    ret
    sub     C2, B0
    sbc     C3, B1
    ret
ENDF __usmulhisi3_tail
#endif /* L_usmulhisi3 */

#if defined (L_umulhisi3)
;;; R25:R22 = (unsigned long) R27:R26 * (unsigned long) R19:R18
;;; C3:C0   = (unsigned long) A1:A0   * (unsigned long) B1:B0
;;; Clobbers: __tmp_reg__
DEFUN __umulhisi3
    mul     A0, B0
    movw    C0, r0
    mul     A1, B1
    movw    C2, r0
    mul     A0, B1
    rcall   1f
    mul     A1, B0
1:  add     C1, r0
    adc     C2, r1
    clr     __zero_reg__
    adc     C3, __zero_reg__
    ret
ENDF __umulhisi3
#endif /* L_umulhisi3 */

/*******************************************************
    Widening Multiplication  32 = 16 x 32
*******************************************************/

#if defined (L_mulshisi3)
;;; R25:R22 = (signed long) R27:R26 * R21:R18
;;; (C3:C0) = (signed long) A1:A0   * B3:B0
;;; Clobbers: __tmp_reg__
DEFUN __mulshisi3
#ifdef __AVR_ERRATA_SKIP_JMP_CALL__
    ;; Some cores have problem skipping 2-word instruction
    tst     A1
    brmi    __mulohisi3
#else
    sbrs    A1, 7
#endif /* __AVR_HAVE_JMP_CALL__ */
    XJMP    __muluhisi3
    ;; FALLTHRU
ENDF __mulshisi3
    
;;; R25:R22 = (one-extended long) R27:R26 * R21:R18
;;; (C3:C0) = (one-extended long) A1:A0   * B3:B0
;;; Clobbers: __tmp_reg__
DEFUN __mulohisi3
    XCALL   __muluhisi3
    ;; One-extend R27:R26 (A1:A0)
    sub     C2, B0
    sbc     C3, B1
    ret
ENDF __mulohisi3
#endif /* L_mulshisi3 */

#if defined (L_muluhisi3)
;;; R25:R22 = (unsigned long) R27:R26 * R21:R18
;;; (C3:C0) = (unsigned long) A1:A0   * B3:B0
;;; Clobbers: __tmp_reg__
DEFUN __muluhisi3
    XCALL   __umulhisi3
    mul     A0, B3
    add     C3, r0
    mul     A1, B2
    add     C3, r0
    mul     A0, B2
    add     C2, r0
    adc     C3, r1
    clr     __zero_reg__
    ret
ENDF __muluhisi3
#endif /* L_muluhisi3 */

/*******************************************************
    Multiplication  32 x 32
*******************************************************/

#if defined (L_mulsi3)
;;; R25:R22 = R25:R22 * R21:R18
;;; (C3:C0) = C3:C0   * B3:B0
;;; Clobbers: R26, R27, __tmp_reg__
DEFUN __mulsi3
    movw    A0, C0
    push    C2
    push    C3
    XCALL   __muluhisi3
    pop     A1
    pop     A0
    ;; A1:A0 now contains the high word of A
    mul     A0, B0
    add     C2, r0
    adc     C3, r1
    mul     A0, B1
    add     C3, r0
    mul     A1, B0
    add     C3, r0
    clr     __zero_reg__
    ret
ENDF __mulsi3
#endif /* L_mulsi3 */

#undef A0
#undef A1

#undef B0
#undef B1
#undef B2
#undef B3

#undef C0
#undef C1
#undef C2
#undef C3

#endif /* __AVR_HAVE_MUL__ */
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

/*******************************************************
       Division 8 / 8 => (result + remainder)
*******************************************************/
#define	r_rem	r25	/* remainder */
#define	r_arg1	r24	/* dividend, quotient */
#define	r_arg2	r22	/* divisor */
#define	r_cnt	r23	/* loop count */

#if defined (L_udivmodqi4)
	.global	__udivmodqi4
	.func	__udivmodqi4
__udivmodqi4:
	sub	r_rem,r_rem	; clear remainder and carry
	ldi	r_cnt,9		; init loop counter
	rjmp	__udivmodqi4_ep	; jump to entry point
__udivmodqi4_loop:
	rol	r_rem		; shift dividend into remainder
	cp	r_rem,r_arg2	; compare remainder & divisor
	brcs	__udivmodqi4_ep	; remainder <= divisor
	sub	r_rem,r_arg2	; restore remainder
__udivmodqi4_ep:
	rol	r_arg1		; shift dividend (with CARRY)
	dec	r_cnt		; decrement loop counter
	brne	__udivmodqi4_loop
	com	r_arg1		; complement result 
				; because C flag was complemented in loop
	ret
	.endfunc
#endif /* defined (L_udivmodqi4) */

#if defined (L_divmodqi4)
	.global	__divmodqi4
	.func	__divmodqi4
__divmodqi4:
        bst     r_arg1,7	; store sign of dividend
        mov     __tmp_reg__,r_arg1
        eor     __tmp_reg__,r_arg2; r0.7 is sign of result
        sbrc	r_arg1,7
	neg     r_arg1		; dividend negative : negate
        sbrc	r_arg2,7
	neg     r_arg2		; divisor negative : negate
	rcall	__udivmodqi4	; do the unsigned div/mod
	brtc	__divmodqi4_1
	neg	r_rem		; correct remainder sign
__divmodqi4_1:
	sbrc	__tmp_reg__,7
	neg	r_arg1		; correct result sign
__divmodqi4_exit:
	ret
	.endfunc
#endif /* defined (L_divmodqi4) */

#undef r_rem
#undef r_arg1
#undef r_arg2
#undef r_cnt
	
		
/*******************************************************
       Division 16 / 16 => (result + remainder)
*******************************************************/
#define	r_remL	r26	/* remainder Low */
#define	r_remH	r27	/* remainder High */

/* return: remainder */
#define	r_arg1L	r24	/* dividend Low */
#define	r_arg1H	r25	/* dividend High */

/* return: quotient */
#define	r_arg2L	r22	/* divisor Low */
#define	r_arg2H	r23	/* divisor High */
	
#define	r_cnt	r21	/* loop count */

#if defined (L_udivmodhi4)
	.global	__udivmodhi4
	.func	__udivmodhi4
__udivmodhi4:
	sub	r_remL,r_remL
	sub	r_remH,r_remH	; clear remainder and carry
	ldi	r_cnt,17	; init loop counter
	rjmp	__udivmodhi4_ep	; jump to entry point
__udivmodhi4_loop:
        rol	r_remL		; shift dividend into remainder
	rol	r_remH
        cp	r_remL,r_arg2L	; compare remainder & divisor
	cpc	r_remH,r_arg2H
        brcs	__udivmodhi4_ep	; remainder < divisor
        sub	r_remL,r_arg2L	; restore remainder
        sbc	r_remH,r_arg2H
__udivmodhi4_ep:
        rol	r_arg1L		; shift dividend (with CARRY)
        rol	r_arg1H
        dec	r_cnt		; decrement loop counter
        brne	__udivmodhi4_loop
	com	r_arg1L
	com	r_arg1H
; div/mod results to return registers, as for the div() function
	mov_l	r_arg2L, r_arg1L	; quotient
	mov_h	r_arg2H, r_arg1H
	mov_l	r_arg1L, r_remL		; remainder
	mov_h	r_arg1H, r_remH
	ret
	.endfunc
#endif /* defined (L_udivmodhi4) */

#if defined (L_divmodhi4)
	.global	__divmodhi4
	.func	__divmodhi4
__divmodhi4:
	.global	_div
_div:
        bst     r_arg1H,7	; store sign of dividend
        mov     __tmp_reg__,r_arg1H
        eor     __tmp_reg__,r_arg2H   ; r0.7 is sign of result
	rcall	__divmodhi4_neg1 ; dividend negative : negate
	sbrc	r_arg2H,7
	rcall	__divmodhi4_neg2 ; divisor negative : negate
	rcall	__udivmodhi4	; do the unsigned div/mod
	rcall	__divmodhi4_neg1 ; correct remainder sign
	tst	__tmp_reg__
	brpl	__divmodhi4_exit
__divmodhi4_neg2:
	com	r_arg2H
	neg	r_arg2L		; correct divisor/result sign
	sbci	r_arg2H,0xff
__divmodhi4_exit:
	ret
__divmodhi4_neg1:
	brtc	__divmodhi4_exit
	com	r_arg1H
	neg	r_arg1L		; correct dividend/remainder sign
	sbci	r_arg1H,0xff
	ret
	.endfunc
#endif /* defined (L_divmodhi4) */

#undef r_remH  
#undef r_remL  
             
#undef r_arg1H 
#undef r_arg1L 
             
#undef r_arg2H 
#undef r_arg2L 
             	
#undef r_cnt   	
	
/*******************************************************
       Division 32 / 32 => (result + remainder)
*******************************************************/
#define	r_remHH	r31	/* remainder High */
#define	r_remHL	r30
#define	r_remH	r27
#define	r_remL	r26	/* remainder Low */

/* return: remainder */
#define	r_arg1HH r25	/* dividend High */
#define	r_arg1HL r24
#define	r_arg1H  r23
#define	r_arg1L  r22	/* dividend Low */

/* return: quotient */
#define	r_arg2HH r21	/* divisor High */
#define	r_arg2HL r20
#define	r_arg2H  r19
#define	r_arg2L  r18	/* divisor Low */
	
#define	r_cnt __zero_reg__  /* loop count (0 after the loop!) */

#if defined (L_udivmodsi4)
	.global	__udivmodsi4
	.func	__udivmodsi4
__udivmodsi4:
	ldi	r_remL, 33	; init loop counter
	mov	r_cnt, r_remL
	sub	r_remL,r_remL
	sub	r_remH,r_remH	; clear remainder and carry
	mov_l	r_remHL, r_remL
	mov_h	r_remHH, r_remH
	rjmp	__udivmodsi4_ep	; jump to entry point
__udivmodsi4_loop:
        rol	r_remL		; shift dividend into remainder
	rol	r_remH
	rol	r_remHL
	rol	r_remHH
        cp	r_remL,r_arg2L	; compare remainder & divisor
	cpc	r_remH,r_arg2H
	cpc	r_remHL,r_arg2HL
	cpc	r_remHH,r_arg2HH
	brcs	__udivmodsi4_ep	; remainder <= divisor
        sub	r_remL,r_arg2L	; restore remainder
        sbc	r_remH,r_arg2H
        sbc	r_remHL,r_arg2HL
        sbc	r_remHH,r_arg2HH
__udivmodsi4_ep:
        rol	r_arg1L		; shift dividend (with CARRY)
        rol	r_arg1H
        rol	r_arg1HL
        rol	r_arg1HH
        dec	r_cnt		; decrement loop counter
        brne	__udivmodsi4_loop
				; __zero_reg__ now restored (r_cnt == 0)
	com	r_arg1L
	com	r_arg1H
	com	r_arg1HL
	com	r_arg1HH
; div/mod results to return registers, as for the ldiv() function
	mov_l	r_arg2L,  r_arg1L	; quotient
	mov_h	r_arg2H,  r_arg1H
	mov_l	r_arg2HL, r_arg1HL
	mov_h	r_arg2HH, r_arg1HH
	mov_l	r_arg1L,  r_remL	; remainder
	mov_h	r_arg1H,  r_remH
	mov_l	r_arg1HL, r_remHL
	mov_h	r_arg1HH, r_remHH
	ret
	.endfunc
#endif /* defined (L_udivmodsi4) */

#if defined (L_divmodsi4)
	.global	__divmodsi4
	.func	__divmodsi4
__divmodsi4:
        bst     r_arg1HH,7	; store sign of dividend
        mov     __tmp_reg__,r_arg1HH
        eor     __tmp_reg__,r_arg2HH   ; r0.7 is sign of result
	rcall	__divmodsi4_neg1 ; dividend negative : negate
	sbrc	r_arg2HH,7
	rcall	__divmodsi4_neg2 ; divisor negative : negate
	rcall	__udivmodsi4	; do the unsigned div/mod
	rcall	__divmodsi4_neg1 ; correct remainder sign
	rol	__tmp_reg__
	brcc	__divmodsi4_exit
__divmodsi4_neg2:
	com	r_arg2HH
	com	r_arg2HL
	com	r_arg2H
	neg	r_arg2L		; correct divisor/quotient sign
	sbci	r_arg2H,0xff
	sbci	r_arg2HL,0xff
	sbci	r_arg2HH,0xff
__divmodsi4_exit:
	ret
__divmodsi4_neg1:
	brtc	__divmodsi4_exit
	com	r_arg1HH
	com	r_arg1HL
	com	r_arg1H
	neg	r_arg1L		; correct dividend/remainder sign
	sbci	r_arg1H, 0xff
	sbci	r_arg1HL,0xff
	sbci	r_arg1HH,0xff
	ret
	.endfunc
#endif /* defined (L_divmodsi4) */

/**********************************
 * This is a prologue subroutine
 **********************************/
#if defined (L_prologue)

	.global	__prologue_saves__
	.func	__prologue_saves__
__prologue_saves__:
	push r2
	push r3
	push r4
	push r5
	push r6
	push r7
	push r8
	push r9
	push r10
	push r11
	push r12
	push r13
	push r14
	push r15
	push r16
	push r17
	push r28
	push r29
	in	r28,__SP_L__
	in	r29,__SP_H__
	sub	r28,r26
	sbc	r29,r27
	in	__tmp_reg__,__SREG__
	cli
	out	__SP_H__,r29
	out	__SREG__,__tmp_reg__
	out	__SP_L__,r28
#if defined (__AVR_HAVE_EIJMP_EICALL__)
	eijmp
#else
	ijmp
#endif

.endfunc
#endif /* defined (L_prologue) */

/*
 * This is an epilogue subroutine
 */
#if defined (L_epilogue)

	.global	__epilogue_restores__
	.func	__epilogue_restores__
__epilogue_restores__:
	ldd	r2,Y+18
	ldd	r3,Y+17
	ldd	r4,Y+16
	ldd	r5,Y+15
	ldd	r6,Y+14
	ldd	r7,Y+13
	ldd	r8,Y+12
	ldd	r9,Y+11
	ldd	r10,Y+10
	ldd	r11,Y+9
	ldd	r12,Y+8
	ldd	r13,Y+7
	ldd	r14,Y+6
	ldd	r15,Y+5
	ldd	r16,Y+4
	ldd	r17,Y+3
	ldd	r26,Y+2
	ldd	r27,Y+1
	add	r28,r30
	adc	r29,__zero_reg__
	in	__tmp_reg__,__SREG__
	cli
	out	__SP_H__,r29
	out	__SREG__,__tmp_reg__
	out	__SP_L__,r28
	mov_l	r28, r26
	mov_h	r29, r27
	ret
.endfunc
#endif /* defined (L_epilogue) */

#ifdef L_exit
	.section .fini9,"ax",@progbits
	.global _exit
	.func	_exit
_exit:
	.weak	exit
exit:
	.endfunc

	/* Code from .fini8 ... .fini1 sections inserted by ld script.  */

	.section .fini0,"ax",@progbits
	cli
__stop_program:
	rjmp	__stop_program
#endif /* defined (L_exit) */

#ifdef L_cleanup
	.weak	_cleanup
	.func	_cleanup
_cleanup:
	ret
.endfunc
#endif /* defined (L_cleanup) */

#ifdef L_tablejump
	.global __tablejump2__
	.func	__tablejump2__
__tablejump2__:
	lsl	r30
	rol	r31
	.global __tablejump__
__tablejump__:
#if defined (__AVR_HAVE_LPMX__)
	lpm	__tmp_reg__, Z+
	lpm	r31, Z
	mov	r30, __tmp_reg__

#if defined (__AVR_HAVE_EIJMP_EICALL__)
	eijmp
#else
	ijmp
#endif

#else
	lpm
	adiw	r30, 1
	push	r0
	lpm
	push	r0
#if defined (__AVR_HAVE_EIJMP_EICALL__)
        push    __zero_reg__
#endif
	ret
#endif
	.endfunc
#endif /* defined (L_tablejump) */

#ifdef L_copy_data
	.section .init4,"ax",@progbits
	.global __do_copy_data
__do_copy_data:
#if defined(__AVR_HAVE_ELPMX__)
	ldi	r17, hi8(__data_end)
	ldi	r26, lo8(__data_start)
	ldi	r27, hi8(__data_start)
	ldi	r30, lo8(__data_load_start)
	ldi	r31, hi8(__data_load_start)
	ldi	r16, hh8(__data_load_start)
	out	__RAMPZ__, r16
	rjmp	.L__do_copy_data_start
.L__do_copy_data_loop:
	elpm	r0, Z+
	st	X+, r0
.L__do_copy_data_start:
	cpi	r26, lo8(__data_end)
	cpc	r27, r17
	brne	.L__do_copy_data_loop
#elif  !defined(__AVR_HAVE_ELPMX__) && defined(__AVR_HAVE_ELPM__)
	ldi	r17, hi8(__data_end)
	ldi	r26, lo8(__data_start)
	ldi	r27, hi8(__data_start)
	ldi	r30, lo8(__data_load_start)
	ldi	r31, hi8(__data_load_start)
	ldi	r16, hh8(__data_load_start - 0x10000)
.L__do_copy_data_carry:
	inc	r16
	out	__RAMPZ__, r16
	rjmp	.L__do_copy_data_start
.L__do_copy_data_loop:
	elpm
	st	X+, r0
	adiw	r30, 1
	brcs	.L__do_copy_data_carry
.L__do_copy_data_start:
	cpi	r26, lo8(__data_end)
	cpc	r27, r17
	brne	.L__do_copy_data_loop
#elif !defined(__AVR_HAVE_ELPMX__) && !defined(__AVR_HAVE_ELPM__)
	ldi	r17, hi8(__data_end)
	ldi	r26, lo8(__data_start)
	ldi	r27, hi8(__data_start)
	ldi	r30, lo8(__data_load_start)
	ldi	r31, hi8(__data_load_start)
	rjmp	.L__do_copy_data_start
.L__do_copy_data_loop:
#if defined (__AVR_HAVE_LPMX__)
	lpm	r0, Z+
#else
	lpm
	adiw	r30, 1
#endif
	st	X+, r0
.L__do_copy_data_start:
	cpi	r26, lo8(__data_end)
	cpc	r27, r17
	brne	.L__do_copy_data_loop
#endif /* !defined(__AVR_HAVE_ELPMX__) && !defined(__AVR_HAVE_ELPM__) */
#endif /* L_copy_data */

/* __do_clear_bss is only necessary if there is anything in .bss section.  */

#ifdef L_clear_bss
	.section .init4,"ax",@progbits
	.global __do_clear_bss
__do_clear_bss:
	ldi	r17, hi8(__bss_end)
	ldi	r26, lo8(__bss_start)
	ldi	r27, hi8(__bss_start)
	rjmp	.do_clear_bss_start
.do_clear_bss_loop:
	st	X+, __zero_reg__
.do_clear_bss_start:
	cpi	r26, lo8(__bss_end)
	cpc	r27, r17
	brne	.do_clear_bss_loop
#endif /* L_clear_bss */

/* __do_global_ctors and __do_global_dtors are only necessary
   if there are any constructors/destructors.  */

#ifdef L_ctors
	.section .init6,"ax",@progbits
	.global	__do_global_ctors
#if defined(__AVR_HAVE_RAMPZ__)
__do_global_ctors:
	ldi	r17, hi8(__ctors_start)
	ldi	r28, lo8(__ctors_end)
	ldi	r29, hi8(__ctors_end)
	ldi	r16, hh8(__ctors_end)
	rjmp	.L__do_global_ctors_start
.L__do_global_ctors_loop:
	sbiw	r28, 2
	sbc     r16, __zero_reg__
	mov_h	r31, r29
	mov_l	r30, r28
	out     __RAMPZ__, r16
	XCALL	__tablejump_elpm__
.L__do_global_ctors_start:
	cpi	r28, lo8(__ctors_start)
	cpc	r29, r17
	ldi	r24, hh8(__ctors_start)
	cpc	r16, r24
	brne	.L__do_global_ctors_loop
#else
__do_global_ctors:
	ldi	r17, hi8(__ctors_start)
	ldi	r28, lo8(__ctors_end)
	ldi	r29, hi8(__ctors_end)
	rjmp	.L__do_global_ctors_start
.L__do_global_ctors_loop:
	sbiw	r28, 2
	mov_h	r31, r29
	mov_l	r30, r28
	XCALL	__tablejump__
.L__do_global_ctors_start:
	cpi	r28, lo8(__ctors_start)
	cpc	r29, r17
	brne	.L__do_global_ctors_loop
#endif /* defined(__AVR_HAVE_RAMPZ__) */
#endif /* L_ctors */

#ifdef L_dtors
	.section .fini6,"ax",@progbits
	.global	__do_global_dtors
#if defined(__AVR_HAVE_RAMPZ__)
__do_global_dtors:
	ldi	r17, hi8(__dtors_end)
	ldi	r28, lo8(__dtors_start)
	ldi	r29, hi8(__dtors_start)
	ldi	r16, hh8(__dtors_start)
	rjmp	.L__do_global_dtors_start
.L__do_global_dtors_loop:
	sbiw	r28, 2
	sbc     r16, __zero_reg__
	mov_h	r31, r29
	mov_l	r30, r28
	out     __RAMPZ__, r16
	XCALL	__tablejump_elpm__
.L__do_global_dtors_start:
	cpi	r28, lo8(__dtors_end)
	cpc	r29, r17
	ldi	r24, hh8(__dtors_end)
	cpc	r16, r24
	brne	.L__do_global_dtors_loop
#else
__do_global_dtors:
	ldi	r17, hi8(__dtors_end)
	ldi	r28, lo8(__dtors_start)
	ldi	r29, hi8(__dtors_start)
	rjmp	.L__do_global_dtors_start
.L__do_global_dtors_loop:
	mov_h	r31, r29
	mov_l	r30, r28
	XCALL	__tablejump__
	adiw	r28, 2
.L__do_global_dtors_start:
	cpi	r28, lo8(__dtors_end)
	cpc	r29, r17
	brne	.L__do_global_dtors_loop
#endif /* defined(__AVR_HAVE_RAMPZ__) */
#endif /* L_dtors */

#ifdef L_tablejump_elpm
	.global __tablejump_elpm__
	.func	__tablejump_elpm__
__tablejump_elpm__:
#if defined (__AVR_HAVE_ELPM__)
#if defined (__AVR_HAVE_LPMX__)
	elpm	__tmp_reg__, Z+
	elpm	r31, Z
	mov	r30, __tmp_reg__
#if defined (__AVR_HAVE_EIJMP_EICALL__)
	eijmp
#else
	ijmp
#endif

#else
	elpm
	adiw	r30, 1
	push	r0
	elpm
	push	r0
#if defined (__AVR_HAVE_EIJMP_EICALL__)
        push    __zero_reg__
#endif
	ret
#endif
#endif /* defined (__AVR_HAVE_ELPM__) */
	.endfunc
#endif /* defined (L_tablejump_elpm) */


/**********************************
 * Find first set Bit (ffs)
 **********************************/

#if defined (L_ffssi2)
;; find first set bit
;; r25:r24 = ffs32 (r25:r22)
;; clobbers: r22, r26
DEFUN __ffssi2
    clr  r26
    tst  r22
    brne 1f
    subi r26, -8
    or   r22, r23
    brne 1f
    subi r26, -8
    or   r22, r24
    brne 1f
    subi r26, -8
    or   r22, r25
    brne 1f
    ret
1:  mov  r24, r22
    XJMP __loop_ffsqi2
ENDF __ffssi2
#endif /* defined (L_ffssi2) */

#if defined (L_ffshi2)
;; find first set bit
;; r25:r24 = ffs16 (r25:r24)
;; clobbers: r26
DEFUN __ffshi2
    clr  r26
#ifdef __AVR_ERRATA_SKIP_JMP_CALL__
    ;; Some cores have problem skipping 2-word instruction
    tst  r24
    breq 2f
#else
    cpse r24, __zero_reg__
#endif /* __AVR_HAVE_JMP_CALL__ */
1:  XJMP __loop_ffsqi2
2:  ldi  r26, 8
    or   r24, r25
    brne 1b
    ret
ENDF __ffshi2
#endif /* defined (L_ffshi2) */

#if defined (L_loop_ffsqi2)
;; Helper for ffshi2, ffssi2
;; r25:r24 = r26 + zero_extend16 (ffs8(r24))
;; r24 must be != 0
;; clobbers: r26
DEFUN __loop_ffsqi2
    inc  r26
    lsr  r24
    brcc __loop_ffsqi2
    mov  r24, r26
    clr  r25
    ret    
ENDF __loop_ffsqi2
#endif /* defined (L_loop_ffsqi2) */


/**********************************
 * Count trailing Zeros (ctz)
 **********************************/

#if defined (L_ctzsi2)
;; count trailing zeros
;; r25:r24 = ctz32 (r25:r22)
;; clobbers: r26, r22
;; ctz(0) = 255
;; Note that ctz(0) in undefined for GCC
DEFUN __ctzsi2
    XCALL __ffssi2
    dec  r24
    ret
ENDF __ctzsi2
#endif /* defined (L_ctzsi2) */

#if defined (L_ctzhi2)
;; count trailing zeros
;; r25:r24 = ctz16 (r25:r24)
;; clobbers: r26
;; ctz(0) = 255
;; Note that ctz(0) in undefined for GCC
DEFUN __ctzhi2
    XCALL __ffshi2
    dec  r24
    ret
ENDF __ctzhi2
#endif /* defined (L_ctzhi2) */


/**********************************
 * Count leading Zeros (clz)
 **********************************/

#if defined (L_clzdi2)
;; count leading zeros
;; r25:r24 = clz64 (r25:r18)
;; clobbers: r22, r23, r26
DEFUN __clzdi2
    XCALL __clzsi2
    sbrs r24, 5
    ret
    mov_l r22, r18
    mov_h r23, r19
    mov_l r24, r20
    mov_h r25, r21
    XCALL __clzsi2
    subi r24, -32
    ret
ENDF __clzdi2
#endif /* defined (L_clzdi2) */

#if defined (L_clzsi2)
;; count leading zeros
;; r25:r24 = clz32 (r25:r22)
;; clobbers: r26
DEFUN __clzsi2
    XCALL __clzhi2
    sbrs r24, 4
    ret
    mov_l r24, r22
    mov_h r25, r23
    XCALL __clzhi2
    subi r24, -16
    ret
ENDF __clzsi2
#endif /* defined (L_clzsi2) */

#if defined (L_clzhi2)
;; count leading zeros
;; r25:r24 = clz16 (r25:r24)
;; clobbers: r26
DEFUN __clzhi2
    clr  r26
    tst  r25
    brne 1f
    subi r26, -8
    or   r25, r24
    brne 1f
    ldi  r24, 16
    ret
1:  cpi  r25, 16
    brsh 3f
    subi r26, -3
    swap r25
2:  inc  r26
3:  lsl  r25
    brcc 2b
    mov  r24, r26
    clr  r25
    ret
ENDF __clzhi2
#endif /* defined (L_clzhi2) */


/**********************************
 * Parity 
 **********************************/

#if defined (L_paritydi2)
;; r25:r24 = parity64 (r25:r18)
;; clobbers: __tmp_reg__
DEFUN __paritydi2
    eor  r24, r18
    eor  r24, r19
    eor  r24, r20
    eor  r24, r21
    XJMP __paritysi2
ENDF __paritydi2
#endif /* defined (L_paritydi2) */

#if defined (L_paritysi2)
;; r25:r24 = parity32 (r25:r22)
;; clobbers: __tmp_reg__
DEFUN __paritysi2
    eor  r24, r22
    eor  r24, r23
    XJMP __parityhi2
ENDF __paritysi2
#endif /* defined (L_paritysi2) */

#if defined (L_parityhi2)
;; r25:r24 = parity16 (r25:r24)
;; clobbers: __tmp_reg__
DEFUN __parityhi2
    eor  r24, r25
;; FALLTHRU
ENDF __parityhi2

;; r25:r24 = parity8 (r24)
;; clobbers: __tmp_reg__
DEFUN __parityqi2
    ;; parity is in r24[0..7]
    mov  __tmp_reg__, r24
    swap __tmp_reg__
    eor  r24, __tmp_reg__
    ;; parity is in r24[0..3]
    subi r24, -4
    andi r24, -5
    subi r24, -6
    ;; parity is in r24[0,3]
    sbrc r24, 3
    inc  r24
    ;; parity is in r24[0]
    andi r24, 1
    clr  r25
    ret
ENDF __parityqi2
#endif /* defined (L_parityhi2) */


/**********************************
 * Population Count
 **********************************/

#if defined (L_popcounthi2)
;; population count
;; r25:r24 = popcount16 (r25:r24)
;; clobbers: __tmp_reg__
DEFUN __popcounthi2
    XCALL __popcountqi2
    push r24
    mov  r24, r25
    XCALL __popcountqi2
    clr  r25
    ;; FALLTHRU
ENDF __popcounthi2

DEFUN __popcounthi2_tail
    pop   __tmp_reg__
    add   r24, __tmp_reg__
    ret
ENDF __popcounthi2_tail
#endif /* defined (L_popcounthi2) */

#if defined (L_popcountsi2)
;; population count
;; r25:r24 = popcount32 (r25:r22)
;; clobbers: __tmp_reg__
DEFUN __popcountsi2
    XCALL __popcounthi2
    push  r24
    mov_l r24, r22
    mov_h r25, r23
    XCALL __popcounthi2
    XJMP  __popcounthi2_tail
ENDF __popcountsi2
#endif /* defined (L_popcountsi2) */

#if defined (L_popcountdi2)
;; population count
;; r25:r24 = popcount64 (r25:r18)
;; clobbers: r22, r23, __tmp_reg__
DEFUN __popcountdi2
    XCALL __popcountsi2
    push  r24
    mov_l r22, r18
    mov_h r23, r19
    mov_l r24, r20
    mov_h r25, r21
    XCALL __popcountsi2
    XJMP  __popcounthi2_tail
ENDF __popcountdi2
#endif /* defined (L_popcountdi2) */

#if defined (L_popcountqi2)
;; population count
;; r24 = popcount8 (r24)
;; clobbers: __tmp_reg__
DEFUN __popcountqi2
    mov  __tmp_reg__, r24
    andi r24, 1
    lsr  __tmp_reg__    
    lsr  __tmp_reg__    
    adc  r24, __zero_reg__
    lsr  __tmp_reg__    
    adc  r24, __zero_reg__
    lsr  __tmp_reg__    
    adc  r24, __zero_reg__
    lsr  __tmp_reg__    
    adc  r24, __zero_reg__
    lsr  __tmp_reg__    
    adc  r24, __zero_reg__
    lsr  __tmp_reg__    
    adc  r24, __tmp_reg__    
    ret    
ENDF __popcountqi2
#endif /* defined (L_popcountqi2) */


/**********************************
 * Swap bytes
 **********************************/

;; swap two registers with different register number
.macro bswap a, b
    eor \a, \b
    eor \b, \a
    eor \a, \b
.endm

#if defined (L_bswapsi2)
;; swap bytes
;; r25:r22 = bswap32 (r25:r22)
DEFUN __bswapsi2
    bswap r22, r25
    bswap r23, r24
    ret
ENDF __bswapsi2
#endif /* defined (L_bswapsi2) */

#if defined (L_bswapdi2)
;; swap bytes
;; r25:r18 = bswap64 (r25:r18)
DEFUN __bswapdi2
    bswap r18, r25
    bswap r19, r24
    bswap r20, r23
    bswap r21, r22
    ret
ENDF __bswapdi2
#endif /* defined (L_bswapdi2) */


/**********************************
 * 64-bit shifts
 **********************************/

#if defined (L_ashrdi3)
;; Arithmetic shift right
;; r25:r18 = ashr64 (r25:r18, r17:r16)
DEFUN __ashrdi3
    push r16
    andi r16, 63
    breq 2f
1:  asr  r25
    ror  r24
    ror  r23
    ror  r22
    ror  r21
    ror  r20
    ror  r19
    ror  r18
    dec  r16
    brne 1b
2:  pop  r16
    ret
ENDF __ashrdi3
#endif /* defined (L_ashrdi3) */

#if defined (L_lshrdi3)
;; Logic shift right
;; r25:r18 = lshr64 (r25:r18, r17:r16)
DEFUN __lshrdi3
    push r16
    andi r16, 63
    breq 2f
1:  lsr  r25
    ror  r24
    ror  r23
    ror  r22
    ror  r21
    ror  r20
    ror  r19
    ror  r18
    dec  r16
    brne 1b
2:  pop  r16
    ret
ENDF __lshrdi3
#endif /* defined (L_lshrdi3) */

#if defined (L_ashldi3)
;; Shift left
;; r25:r18 = ashl64 (r25:r18, r17:r16)
DEFUN __ashldi3
    push r16
    andi r16, 63
    breq 2f
1:  lsl  r18
    rol  r19
    rol  r20
    rol  r21
    rol  r22
    rol  r23
    rol  r24
    rol  r25
    dec  r16
    brne 1b
2:  pop  r16
    ret
ENDF __ashldi3
#endif /* defined (L_ashldi3) */


/***********************************************************/    
;;; Softmul versions of FMUL, FMULS and FMULSU to implement
;;; __builtin_avr_fmul* if !AVR_HAVE_MUL
/***********************************************************/    

#define A1 24
#define B1 25
#define C0 22
#define C1 23
#define A0 __tmp_reg__

#ifdef L_fmuls
;;; r23:r22 = fmuls (r24, r25) like in FMULS instruction
;;; Clobbers: r24, r25, __tmp_reg__
DEFUN __fmuls
    ;; A0.7 = negate result?
    mov  A0, A1
    eor  A0, B1
    ;; B1 = |B1|
    sbrc B1, 7
    neg  B1
    XJMP __fmulsu_exit
ENDF __fmuls
#endif /* L_fmuls */

#ifdef L_fmulsu
;;; r23:r22 = fmulsu (r24, r25) like in FMULSU instruction
;;; Clobbers: r24, r25, __tmp_reg__
DEFUN __fmulsu
    ;; A0.7 = negate result?
    mov  A0, A1
;; FALLTHRU
ENDF __fmulsu

;; Helper for __fmuls and __fmulsu
DEFUN __fmulsu_exit
    ;; A1 = |A1|
    sbrc A1, 7
    neg  A1
#ifdef __AVR_ERRATA_SKIP_JMP_CALL__
    ;; Some cores have problem skipping 2-word instruction
    tst  A0
    brmi 1f
#else
    sbrs A0, 7
#endif /* __AVR_HAVE_JMP_CALL__ */
    XJMP  __fmul
1:  XCALL __fmul
    ;; C = -C iff A0.7 = 1
    com  C1
    neg  C0
    sbci C1, -1
    ret
ENDF __fmulsu_exit
#endif /* L_fmulsu */


#ifdef L_fmul
;;; r22:r23 = fmul (r24, r25) like in FMUL instruction
;;; Clobbers: r24, r25, __tmp_reg__
DEFUN __fmul
    ; clear result
    clr   C0
    clr   C1
    clr   A0
1:  tst   B1
    ;; 1.0 = 0x80, so test for bit 7 of B to see if A must to be added to C.
2:  brpl  3f
    ;; C += A
    add   C0, A0
    adc   C1, A1
3:  ;; A >>= 1
    lsr   A1
    ror   A0
    ;; B <<= 1
    lsl   B1
    brne  2b
    ret
ENDF __fmul
#endif /* L_fmul */

#undef A0
#undef A1
#undef B1
#undef C0
#undef C1
