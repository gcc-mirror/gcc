/*  -*- Mode: Asm -*-  */
/* Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Denis Chertykov <denisc@overta.ru>

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

#define TEXT_SEG(x) .section .text.libgcc ; x
#define GLOBAL(x) .global _##x
#define FUNCTION(x) .func _##x
#define LABEL(x) _##x##:
#define ENDFUNC .endfunc

#define __zero_reg__ r1
#define __tmp_reg__ r0
#define __SREG__ 0x3f
#define __SP_H__ 0x3e
#define __SP_L__ 0x3d

/*******************************************************
               Multiplication  8 x 8
*******************************************************/
#if defined (Lmulqi3)

#define	r_arg2	r25		/* multiplicand */
#define	r_arg1 	r24		/* multiplier */
#define r_res	__tmp_reg__	/* result */

TEXT_SEG(mulqi3)
GLOBAL (mulqi3)
FUNCTION (mulqi3)
LABEL(mulqi3)

GLOBAL (umulqi3)
LABEL(umulqi3)
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

#undef r_arg2  
#undef r_arg1  
#undef r_res   
	
ENDFUNC
#endif 	/* defined (Lmulqi3) */


/*******************************************************
               Multiplication  16 x 16
*******************************************************/
#if defined (Lmulhi3)
#define	r_arg1L	r24		/* multiplier Low */
#define	r_arg1H	r25		/* multiplier High */
#define	r_arg2L	r22		/* multiplicand Low */
#define	r_arg2H	r23		/* multiplicand High */
#define r_resL	r20		/* result Low */
#define r_resH  r21		/* result High */

TEXT_SEG(mulhi3)
GLOBAL (mulhi3)
FUNCTION (mulhi3)
LABEL(mulhi3)

GLOBAL (umulhi3)
LABEL(umulhi3)
	
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

	cpc	r_arg2L,__zero_reg__
	breq	__mulhi3_exit	; while multiplicand != 0

	lsr	r_arg1H		; gets LSB of multiplier
	ror	r_arg1L
	cpc	r_arg1H,__zero_reg__
	brne	__mulhi3_loop	; exit if multiplier = 0
__mulhi3_exit:
	mov	r_arg1H,r_resH	; result to return register
	mov	r_arg1L,r_resL
	ret

#undef r_arg1L
#undef r_arg1H
#undef r_arg2L
#undef r_arg2H
#undef r_resL 	
#undef r_resH 

ENDFUNC
#endif /* defined (Lmulhi3) */

#if defined (Lmulsi3)
/*******************************************************
               Multiplication  32 x 32
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

	
TEXT_SEG(mulsi3)
GLOBAL (mulsi3)
FUNCTION (mulsi3)
LABEL(mulsi3)

GLOBAL (umulsi3)
LABEL(umulsi3)
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
	mov	r_arg1HH,r_resHH	; result to return register
	mov	r_arg1HL,r_resHL
	mov	r_arg1H,r_resH
	mov	r_arg1L,r_resL
	ret
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

ENDFUNC
#endif /* defined (Lmulsi3) */
	
/*******************************************************
       Division 8 / 8 => (result + remainder)
*******************************************************/
#define	r_rem	r26	/* remainder */
#define	r_arg1	r25	/* dividend */
#define	r_arg2	r24	/* divisor */
#define	r_cnt	r27	/* loop count */

#if defined (Lumodqi3)

TEXT_SEG(divqi3)
GLOBAL (umodqi3)
FUNCTION (umodqi3)
LABEL(umodqi3)
	clt
	rcall	_udivqi3
	mov	r24,r_rem
	ret
ENDFUNC
#endif /* defined (Lumodqi3) */
	
#if defined (Ludivqi3)

TEXT_SEG(divqi3)
GLOBAL (udivqi3)
FUNCTION (udivqi3)
LABEL(udivqi3)
	clr	__tmp_reg__
	rjmp	_divqi_raw
ENDFUNC
#endif /* defined (Ludivqi3) */

#if defined (Lmodqi3)
	
TEXT_SEG (divqi3)
GLOBAL (moqhi3)
FUNCTION (moqhi3)
LABEL (modqi3)
	rcall	_divqi3
	mov	r24,r_rem
	ret
ENDFUNC
#endif /* defined (Lmodqi3) */

#if defined (Ldivqi3)

TEXT_SEG(divqi3)
GLOBAL (divqi3)
FUNCTION (divqi3)
LABEL(divqi3)
        bst     r_arg1,7	; store sign of divident
        mov     __tmp_reg__,r_arg1
        eor     __tmp_reg__,r_arg2; r0.7 is sign of result
        sbrc	r_arg1,7
        neg     r_arg1		; divident negative : negate
        sbrc	r_arg2,7
        neg     r_arg2		; divisor negative : negate
GLOBAL (divqi_raw)
LABEL (divqi_raw)	
	sub	r_rem,r_rem	; clear remainder and carry
	ldi	r_cnt,9		; init loop counter
	rjmp	__divqi3_ep	; jump to entry point
__divqi3_loop:
        rol	r_rem		; shift dividend into remainder
        cp	r_rem,r_arg2	; compare remainder & divisor
        brcs	__divqi3_ep	; remainder <= divisor
        sub	r_rem,r_arg2	; restore remainder
__divqi3_ep:
        rol	r_arg1		; shift dividend (with CARRY)
        dec	r_cnt		; decrement loop counter
        brne	__divqi3_loop	; loop
	com	r_arg1		; complement result 
				; because C flag was complemented in loop
	brtc	__divqi3_1
	neg	r_rem		; correct remainder sign
__divqi3_1:
	sbrc	__tmp_reg__,7
	neg	r_arg1		; correct result sign
__divqi3_exit:
	mov	r24,r_arg1	; put result to return register
	ret
ENDFUNC
#endif /* defined (Ldivqi3) */

#undef r_rem
#undef r_arg1
#undef r_arg2
#undef r_cnt
	
		
/*******************************************************
       Division 16 / 16 => (result + remainder)
*******************************************************/
#define	r_remL	r26	/* remainder Low */
#define	r_remH	r27	/* remainder High */
	
#define	r_arg1L	r24	/* dividend Low */
#define	r_arg1H	r25	/* dividend High */
	
#define	r_arg2L	r22	/* divisor Low */
#define	r_arg2H	r23	/* divisor High */
	
#define	r_cnt	r21	/* loop count */
#if defined (Lumodhi3)
	
TEXT_SEG (divhi3)
GLOBAL (umodhi3)
FUNCTION (umodhi3)
LABEL (umodhi3)
	clt
	rcall	_udivhi3
GLOBAL (umodhi3_ret)
LABEL (umodhi3_ret)
	mov	r24,r_remL
	mov	r25,r_remH
	ret
ENDFUNC
#endif /* defined (Lumodhi3) */
	
#if defined (Ludivhi3)
	
TEXT_SEG (divhi3)
GLOBAL (udivhi3)
FUNCTION (udivhi3)
LABEL (udivhi3)
	clr	__tmp_reg__
	rjmp	_divhi_raw
ENDFUNC
#endif /* defined (Ludivhi3) */

#if defined (Lmodhi3)
	
TEXT_SEG (divhi3)
GLOBAL (modhi3)
FUNCTION (modhi3)
LABEL (modhi3)
GLOBAL (div)
LABEL (div)
	rcall	_divhi3
	mov	r22,r24		; needed for div () function
	mov	r23,r25
	rjmp	_umodhi3_ret
ENDFUNC
#endif /* defined (Lmodhi3) */
	
	
#if defined (Ldivhi3)
	
TEXT_SEG (divhi3)
GLOBAL (divhi3)
FUNCTION (divhi3)
LABEL (divhi3)
        bst     r_arg1H,7	; store sign of divident
        mov     __tmp_reg__,r_arg1H
        eor     __tmp_reg__,r_arg2H   ; r0.7 is sign of result
	brtc	__divhi3_skip1
	com	r_arg1H
        neg     r_arg1L		; divident negative : negate
	sbci	r_arg1H,0xff
__divhi3_skip1:
        tst	r_arg2H
	brpl	__divhi3_skip2
	com	r_arg2H
        neg     r_arg2L		; divisor negative : negate
	sbci	r_arg2H,0xff
__divhi3_skip2:
GLOBAL (divhi_raw)
LABEL (divhi_raw)
	sub	r_remL,r_remL
	sub	r_remH,r_remH		; clear remainder and carry
	ldi	r_cnt,17	; init loop counter
	rjmp	__divhi3_ep	; jump to entry point
__divhi3_loop:
        rol	r_remL		; shift dividend into remainder
	rol	r_remH
        cp	r_remL,r_arg2L	; compare remainder & divisor
	cpc	r_remH,r_arg2H
        brcs	__divhi3_ep	; remainder < divisor
        sub	r_remL,r_arg2L	; restore remainder
        sbc	r_remH,r_arg2H
__divhi3_ep:
        rol	r_arg1L		; shift dividend (with CARRY)
        rol	r_arg1H
        dec	r_cnt		; decrement loop counter
        brne	__divhi3_loop	; loop
	brtc	__divhi3_1
	com	r_remH
	neg	r_remL		; correct remainder sign
	sbci	r_remH,0xff
__divhi3_1:
	tst	__tmp_reg__
	brpl	__divhi3_exit
	adiw	r_arg1L,1	; correct result sign
	ret
__divhi3_exit:
	com	r_arg1L
	com	r_arg1H
	ret
ENDFUNC
#endif /* defined (Ldivhi3) */
	
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
	
#define	r_arg1HH r25	/* dividend High */
#define	r_arg1HL r24
#define	r_arg1H  r23
#define	r_arg1L  r22	/* dividend Low */
	
#define	r_arg2HH r21	/* divisor High */
#define	r_arg2HL r20
#define	r_arg2H  r19
#define	r_arg2L  r18	/* divisor Low */
	
#define	r_cnt	r17	/* loop count */

#if defined (Lumodsi3)

TEXT_SEG(divsi3)
GLOBAL (umodsi3)
FUNCTION (umodsi3)
LABEL(umodsi3)
	clt
	rcall	_udivsi3
GLOBAL (umodsi3_ret)
LABEL (umodsi3_ret)
	mov	r25,r_remHH
	mov	r24,r_remHL
	mov	r23,r_remH
	mov	r22,r_remL
GLOBAL (cleanup)
LABEL (cleanup)
	ret
ENDFUNC
#endif /* defined (Lumodsi3) */
	
#if defined (Ludivsi3)

TEXT_SEG(divsi3)
GLOBAL (udivsi3)
FUNCTION (udivsi3)
LABEL(udivsi3)
	clr	__tmp_reg__
	rjmp	_divsi_raw
ENDFUNC
#endif /* defined (Ludivsi3) */

#if defined (Lmodsi3)
	
TEXT_SEG (divsi3)
GLOBAL (modsi3)
FUNCTION (modsi3)
LABEL (modsi3)
GLOBAL (ldiv)
LABEL (ldiv)
	rcall	_divsi3
	mov	r18,r22		/* Needed for ldiv */
	mov	r19,r23
	mov	r20,r24
	mov	r21,r25
	rjmp	_umodsi3_ret
ENDFUNC
#endif /* defined (Lmodsi3) */

#if defined (Ldivsi3)

TEXT_SEG(divsi3)
GLOBAL (divsi3)
FUNCTION (divsi3)
LABEL(divsi3)
        bst     r_arg1HH,7	; store sign of divident
        mov     __tmp_reg__,r_arg1HH
        eor     __tmp_reg__,r_arg2HH   ; r0.7 is sign of result
	brtc	__divsi3_skip1
	com	r_arg1HH
	com	r_arg1HL
	com	r_arg1H
        neg     r_arg1L		; divident negative : negate
	sbci	r_arg1H, 0xff
	sbci	r_arg1HL,0xff
	sbci	r_arg1HH,0xff
__divsi3_skip1:
        tst	r_arg2HH
	brpl	__divsi3_skip2
	com	r_arg2HH
	com	r_arg2HL
	com	r_arg2H
        neg     r_arg2L		; divisor negative : negate
	sbci	r_arg2H, 0xff
	sbci	r_arg2HL,0xff
	sbci	r_arg2HH,0xff
__divsi3_skip2:
GLOBAL (divsi_raw)
LABEL (divsi_raw)
	push	r_cnt
	sub	r_remL,r_remL
	sub	r_remH,r_remH
	sub	r_remHL,r_remHL
	sub	r_remHH,r_remHH	; clear remainder and carry
	ldi	r_cnt,33	; init loop counter
	rjmp	__divsi3_ep	; jump to entry point
__divsi3_loop:
        rol	r_remL		; shift dividend into remainder
	rol	r_remH
	rol	r_remHL
	rol	r_remHH
        cp	r_remL,r_arg2L	; compare remainder & divisor
	cpc	r_remH,r_arg2H
	cpc	r_remHL,r_arg2HL
	cpc	r_remHH,r_arg2HH
        brcs	__divsi3_ep	; remainder <= divisor
        sub	r_remL,r_arg2L	; restore remainder
        sbc	r_remH,r_arg2H
        sbc	r_remHL,r_arg2HL
        sbc	r_remHH,r_arg2HH
__divsi3_ep:
        rol	r_arg1L		; shift dividend (with CARRY)
        rol	r_arg1H
        rol	r_arg1HL
        rol	r_arg1HH
        dec	r_cnt		; decrement loop counter
        brne	__divsi3_loop	; loop
	pop	r_cnt
	brtc	__divsi3_1
	com	r_remHH
	com	r_remHL
	com	r_remH
	neg	r_remL		; correct remainder sign
	sbci	r_remH, 0xff
	sbci	r_remHL,0xff
	sbci	r_remHH,0xff
__divsi3_1:
	rol	__tmp_reg__
	brcc	__divsi3_exit
	adc	r_arg1L,__zero_reg__; correct result sign
	adc	r_arg1H,__zero_reg__
	adc	r_arg1HL,__zero_reg__
	adc	r_arg1HH,__zero_reg__
	ret
__divsi3_exit:
	com	r_arg1L
	com	r_arg1H
	com	r_arg1HL
	com	r_arg1HH
	ret
ENDFUNC
#endif /* defined (Ldivsi3) */

/**********************************
 * This is a prologue subroutine
 **********************************/
#if defined (Lprologue)

TEXT_SEG (_prologue_saves)
GLOBAL (_prologue_saves__)
FUNCTION (_prologue_saves__)
LABEL (_prologue_saves__)
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
	sbiw r26,0
	breq _prologue_end
	sub	r28,r26
	sbc	r29,r27
	in	__tmp_reg__,__SREG__
	cli
	out	__SP_L__,r28
	out	__SREG__,__tmp_reg__
	out	__SP_H__,r29
_prologue_end:
	ijmp
ENDFUNC
#endif /* defined (Lprologue) */

/*
 * This is a epilogue subroutine
 */
#if defined (Lepilogue)

TEXT_SEG (_epilogue_restores)
GLOBAL (_epilogue_restores__)
FUNCTION (_epilogue_restores__)
LABEL (_epilogue_restores__)
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
	out	__SP_L__,r28
	out	__SREG__,__tmp_reg__
	out	__SP_H__,r29
	mov	r28,r26
	mov	r29,r27
	ret
#endif /* defined (Lepilogue) */

#ifdef L__exit
TEXT_SEG(exit)
GLOBAL (exit)
FUNCTION (exit)
LABEL(exit)
	rjmp	_exit
ENDFUNC
#endif