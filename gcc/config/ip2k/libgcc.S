;
; Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
; Contributed by Red Hat, Inc and Ubicom, Inc.
;
; This file is part of GNU CC.
;
; GNU CC is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2, or (at your option)
; any later version.
;
; In addition to the permissions in the GNU General Public License, the
; Free Software Foundation gives you unlimited permission to link the
; compiled version of this file with other programs, and to distribute
; those programs without any restriction coming from the use of this
; file.  (The General Public License restrictions do apply in other
; respects; for example, they cover modification of the file, and
; distribution when not linked into another program.)
;
; GNU CC is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with GNU CC; see the file COPYING.  If not, write to
; the Free Software Foundation, 59 Temple Place - Suite 330,
; Boston, MA 02111-1307, USA.  */

/*******************************************************
	load byte from arbitrary memory
	address passed in first bank register, result in W

*******************************************************/
	.macro	movb	to, from
	mov	w, \from
	mov	\to, w
	.endm


#if defined (L_indcall)
/* __indcall - given register containing an address, call the function
 *	at that address.
 */

	.sect	.pram.libgcc,"ax"
	.global	__indcall
	.func	_indcall,__indcall

__indcall:
	page	1f
	call	1f
1:	pop	callh			; Get the call target
	pop	calll
	ret				; Transfer to new function

	.endfunc
#endif


#if defined (L_mulhi3)
	.sect	.pram.libgcc,"ax"
	.global	__mulhi3
	.func	_mulhi3, __mulhi3

__mulhi3:
	mov	w, 2(SP)		; First upper half partial product
	mulu	w, 3(SP)
	mov	3(SP), w
	mov	w, 1(SP)		; Second upper half partial product
	mulu	w, 4(SP)
	add	3(SP), w
	mov	w, 2(SP)		; Lower half partial product
	mulu	w, 4(SP)
	mov	4(SP), w
	mov	w, MULH
	add	3(SP), w

	mov	w, #2			; Adjust the stack leaving the result to
	add	spl, w			; be popped off later.
	ret

	.endfunc

#endif /* defined (L_mulhi3) */

#if defined (L_mulsi3)
/*******************************************************
               Multiplication  32 x 32
*******************************************************/
	
	.sect	.text.libgcc,"ax"
	.global	__mulsi3
	.func	_mulsi3, __mulsi3

__mulsi3:
	clr	$80			; Assume zero result
	clr	$81
	clr	$82
	clr	$83

2:	mov	w, 1(sp)
	or	w, 2(sp)
	or	w, 3(sp)
	or	w, 4(sp)
	snz				; Any more significant bits to multiply?
	page	3f
	jmp	3f

	sb	4(sp), 0		; Check LSB of multiplier
	page	1f			; zero => scale multiplicand & multiplier
	jmp	1f

	mov	w, 8(sp)		; Accumulate product
	add	$83, w
	mov	w, 7(sp)
	addc	$82, w
	mov	w, 6(sp)
	addc	$81, w
	mov	w, 5(sp)
	addc	$80, w
1:	clrb	status, 0		; scale multiplier down
	rr	1(sp)	
	rr	2(sp)
	rr	3(sp)
	rr	4(sp)
	clrb	status, 0
	rl	8(sp)
	rl	7(sp)
	rl	6(sp)
	rl	5(sp)
	page	2b
	jmp	2b

3:	mov	w, #8
	add	spl ,w
	ret

	.endfunc

#endif /* defined (L_mulsi3) */
	
#if defined (L_muldi3)
/*******************************************************
               Multiplication  64 x 64
*******************************************************/
	
	.sect	.text.libgcc,"ax"
	.global	__muldi3
	.func	_muldi3, __muldi3

__muldi3:
	clr	$80			; Assume zero result
	clr	$81
	clr	$82
	clr	$83
	clr	$84
	clr	$85
	clr	$86
	clr	$87

2:	mov	w, 1(sp)
	or	w, 2(sp)
	or	w, 3(sp)
	or	w, 4(sp)
	or	w, 5(sp)
	or	w, 6(sp)
	or	w, 7(sp)
	or	w, 8(sp)
	snz				; Any more significant bits to multiply?
	page	3f
	jmp	3f

	sb	8(sp), 0		; Check LSB of multiplier
	page	1f			; zero => scale multiplicand & multiplier
	jmp	1f

	mov	w, 16(sp)		; Accumulate product
	add	$87, w
	mov	w, 15(sp)
	addc	$86, w
	mov	w, 14(sp)
	addc	$85, w
	mov	w, 13(sp)
	addc	$84, w
	mov	w, 12(sp)
	addc	$83, w
	mov	w, 11(sp)
	addc	$82, w
	mov	w, 10(sp)
	addc	$81, w
	mov	w, 9(sp)
	addc	$80, w

1:	clrb	status, 0		; scale multiplier down
	rr	1(sp)	
	rr	2(sp)
	rr	3(sp)
	rr	4(sp)
	rr	5(sp)
	rr	6(sp)
	rr	7(sp)
	rr	8(sp)
	clrb	status, 0
	rl	16(sp)
	rl	15(sp)
	rl	14(sp)
	rl	13(sp)
	rl	12(sp)
	rl	11(sp)
	rl	10(sp)
	rl	9(sp)
	page	2b
	jmp	2b

3:	mov	w, #16
	add	spl, w
	ret

	.endfunc

#endif /* defined (L_muldi3) */

#if defined (L_divmodhi4)
#define	arg1h 1(SP)
#define arg1l 2(SP)
#define arg2h 3(SP)
#define arg2l 4(SP)
#define resl $81
#define resh $80
#define reml $83
#define remh $82
#define tmp_var	$84
#define cnt $85
#define arg1_sign $86
#define res_sign $87

	.sect	.text.libgcc,"ax"
	.global	__divmodhi4
	.func	_divmodhi4, __divmodhi4

__divmodhi4:
	mov	w,arg2h
	mov	res_sign,w
	mov	w,arg1h
	mov	arg1_sign,w
	xor	res_sign,w

	sb	arg1h,7
	page	1f
	jmp	1f

	not	arg1h
	not	arg1l
	incsnz	arg1l
	inc	arg1h

1:	sb	arg2h, 7
	page	1f
	jmp	1f

	not	arg2h
	not	arg2l
	incsnz	arg2l
	inc	arg2h

1:	page	__udivmodhi4		; Do the unsigned div/mod
	call	__udivmodhi4

	sb	arg1_sign, 7
	page	1f
	jmp	1f

	not	reml
	not	remh
	incsnz	reml
	inc	remh

1:	sb	res_sign, 7
	ret

	not	resl
	not	resh
	incsnz	resl
	inc	resh
	ret

	.endfunc

#undef arg1h
#undef arg1l
#undef arg2h
#undef arg2l
#undef resl
#undef resh
#undef reml
#undef remh
#undef tmp_var
#undef cnt
#undef arg1_sign
#undef res_sign

#endif /* defined (L_divmodhi4) */
	
#if defined (L_udivmodhi4)

#define	arg1h 1(SP)
#define arg1l 2(SP)
#define arg2h 3(SP)
#define arg2l 4(SP)
#define resl $81
#define resh $80
#define reml $83
#define remh $82
#define tmp_var	$84
#define cnt $85

	.sect	.text.libgcc,"ax"
	.global	__udivmodhi4
	.func	_udivmodhi4, __udivmodhi4

__udivmodhi4:
	clr	reml
	clr	remh
	mov	w, #17
	mov	cnt,w
	clrb	status, 0
	page	1f
	jmp	1f

2:	rl	reml
	rl	remh
	mov	w, arg2l
	sub	w, reml
	mov	tmp_var, w
	mov	w, arg2h
	subc	w, remh
	sc
	page	1f
	jmp	1f
	mov	remh, w
	mov	w, tmp_var
	mov	reml, w

1:	rl	arg1l
	rl	arg1h
	decsz	cnt
	page	2b
	jmp	2b
	
	pop	resh
	pop	resl	
	mov	w, #2
	add	spl, w
	ret

	.endfunc

#undef arg1h
#undef arg1l
#undef arg2h
#undef arg2l
#undef resl
#undef resh
#undef reml
#undef remh
#undef tmp_var
#undef cnt

#endif	/* defined (L_udivmodhi4) */

#if defined (L_divmodsi4)

#define	arg1a 1(SP)
#define arg1b 2(SP)
#define arg1c 3(SP)
#define arg1d 4(SP)

#define arg2a 5(SP)
#define arg2b 6(SP)
#define arg2c 7(SP)
#define arg2d 8(SP)
	
#define resa $80
#define resb $81
#define resc $82
#define resd $83

#define rema $84
#define remb $85
#define remc $86
#define remd $87

#define tmp_var	$88
#define tmp_var1 $89
#define tmp_var2 $8a
#define cnt $8b
#define arg1_sign $8c
#define res_sign $8d

	.sect	.text.libgcc,"ax"
	.global	__divmodsi4
	.func	_divmodsi4, __divmodsi4

__divmodsi4:
	mov	w, arg2a
	mov	res_sign, w
	mov	w, arg1a
	mov	arg1_sign, w
	xor	res_sign, w

	sb	arg1a, 7
	page	1f
	jmp	1f

	not	arg1d
	not	arg1c
	not	arg1b
	not	arg1a
	incsnz	arg1d
	incsz	arg1c
	page	1f
	jmp	1f
	incsnz	arg1b
	inc	arg1a

1:	sb	arg2a, 7
	page	1f
	jmp	1f

	not	arg2d
	not	arg2c
	not	arg2b
	not	arg2a
	incsnz	arg2d
	incsz	arg2c
	page	1f
	jmp	1f
	incsnz	arg2b
	inc	arg2a

1:	page	__udivmodsi4		; Do the unsigned div/mod.
	call	__udivmodsi4

	sb	arg1_sign, 7
	page	1f
	jmp	1f

	not	remd
	not	remc
	not	remb
	not	rema
	incsnz	remd
	incsz	remc
	page	1f
	jmp	1f
	incsnz	remb
	inc	rema

1:	sb	res_sign, 7
	ret

	not	resd
	not	resc
	not	resb
	not	resa
	incsnz	resd
	incsz	resc
	ret
	incsnz	resb
	inc	resa
	ret

	.endfunc

#undef arg1a
#undef arg1b
#undef arg1c
#undef arg1d

#undef arg2a
#undef arg2b
#undef arg2c
#undef arg2d
	
#undef resa
#undef resb
#undef resc
#undef resd

#undef rema
#undef remb
#undef remc
#undef remd

#undef tmp_var
#undef tmp_var1
#undef tmp_var2
#undef cnt
#undef arg1_sign
#undef res_sign

#endif /* defined (L_divmodsi4) */

#if defined (L_udivmodsi4)		
#define	arg1a 1(SP)
#define arg1b 2(SP)
#define arg1c 3(SP)
#define arg1d 4(SP)

#define arg2a 5(SP)
#define arg2b 6(SP)
#define arg2c 7(SP)
#define arg2d 8(SP)
	
#define resa $80
#define resb $81
#define resc $82
#define resd $83

#define rema $84
#define remb $85
#define remc $86
#define remd $87

#define tmp_var	$88
#define tmp_var1 $89
#define tmp_var2 $8a
#define cnt $8b

	.sect	.text.libgcc,"ax"
	.global	__udivmodsi4
	.func	_udivmodsi4, __udivmodsi4

__udivmodsi4:
	clr	remd
	clr	remc
	clr	remb
	clr	rema
	mov	w, #33
	mov	cnt, w
	clrb	status, 0
	page	1f
	jmp	1f

2:	rl	remd
	rl	remc
	rl	remb
	rl	rema
	mov	w, arg2d
	sub	w, remd
	mov	tmp_var, w
	mov	w, arg2c
	subc	w, remc
	mov	tmp_var1, w
	mov	w, arg2b
	subc	w, remb
	mov	tmp_var2, w
	mov	w, arg2a
	subc	w, rema
	sc
	page	1f
	jmp	1f

	mov	rema, w
	mov	w, tmp_var2
	mov	remb, w
	mov	w, tmp_var1
	mov	remc, w
	mov	w, tmp_var
	mov	remd, w

1:	rl	arg1d
	rl	arg1c
	rl	arg1b
	rl	arg1a
	decsz	cnt
	page	2b
	jmp	2b

	pop	resa
	pop	resb
	pop	resc
	pop	resd
	mov	w, #4
	add	spl, w
	ret

	.endfunc

#undef arg1a
#undef arg1b
#undef arg1c
#undef arg1d

#undef arg2a
#undef arg2b
#undef arg2c
#undef arg2d
	
#undef resa
#undef resb
#undef resc
#undef resd

#undef rema
#undef remb
#undef remc
#undef remd

#undef tmp_var
#undef tmp_var1
#undef tmp_var2
#undef cnt

#endif /* defined (L_udivmodsi4) */

#if defined (L_divmoddi4)

#define	arg1s 1(SP)
#define arg1t 2(SP)
#define arg1u 3(SP)
#define arg1v 4(SP)
#define arg1w 5(SP)
#define arg1x 6(SP)
#define arg1y 7(SP)
#define arg1z 8(SP)

#define arg2s 9(SP)
#define arg2t 10(SP)
#define arg2u 11(SP)
#define arg2v 12(SP)
#define arg2w 13(SP)
#define arg2x 14(SP)
#define arg2y 15(SP)
#define arg2z 16(SP)
	
#define ress $80
#define rest $81
#define resu $82
#define resv $83
#define resw $84
#define resx $85
#define resy $86
#define resz $87

#define rems $88
#define remt $89
#define remu $8a
#define remv $8b
#define remw $8c
#define remx $8d
#define remy $8e
#define remz $8f

#define tmp_var	$90
#define tmp_var1 $91
#define tmp_var2 $92
#define tmp_var3 $93
#define tmp_var4 $94
#define tmp_var5 $95
#define tmp_var6 $96
#define cnt $97

	.sect	.text.libgcc,"ax"
	.global	__divmoddi4
	.func	_divmoddi4, __divmoddi4

__divmoddi4:
	rl	w, arg2s		; Use MULH to track sign bits.
	rl	MULH
	rl	w, arg1s
	rl	WREG
	xor	MULH, w
	rl	w, arg1s
	rl	MULH

	sb	arg1s, 7
	page	1f
	jmp	1f

	not	arg1s
	not	arg1t
	not	arg1u
	not	arg1v
	not	arg1w
	not	arg1x
	not	arg1y
	not	arg1z
	incsnz	arg1z
	incsz	arg1y
	page	1f
	jmp	1f
	incsnz	arg1x
	incsz	arg1w
	page	1f
	jmp	1f
	incsnz	arg1v
	incsz	arg1u
	page	1f
	jmp	1f
	incsnz	arg1t
	inc	arg1s

1:	sb	arg2s, 7
	page	1f
	jmp	1f

	not	arg2s
	not	arg2t
	not	arg2u
	not	arg2v
	not	arg2w
	not	arg2x
	not	arg2y
	not	arg2z
	incsnz	arg2z
	incsz	arg2y
	page	1f
	jmp	1f
	incsnz	arg2x
	incsz	arg2w
	page	1f
	jmp	1f
	incsnz	arg2v
	incsz	arg2u
	page	1f
	jmp	1f
	incsnz	arg2t
	inc	arg2s

1:	page	__udivmoddi4		; Do the unsigned div/mod.
	call	__udivmoddi4

	sb	MULH, 0			; Look at the save sign bit for arg 1.
	page	1f
	jmp	1f

	not	rems
	not	remt
	not	remu
	not	remv
	not	remw
	not	remx
	not	remy
	not	remz
	incsnz	remz
	incsz	remy
	page	1f
	jmp	1f
	incsnz	remx
	incsz	remw
	page	1f
	jmp	1f
	incsnz	remv
	incsz	remu
	page	1f
	jmp	1f
	incsnz	remt
	inc	rems

1:	sb	MULH, 1
	ret

	not	ress
	not	rest
	not	resu
	not	resv
	not	resw
	not	resx
	not	resy
	not	resz
	incsnz	resz
	incsz	resy
	ret
	incsnz	resx
	incsz	resw
	ret
	incsnz	resv
	incsz	resu
	ret
	incsnz	rest
	inc	ress
	ret

	.endfunc

#undef arg1s
#undef arg1t
#undef arg1u
#undef arg1v
#undef arg1w
#undef arg1x
#undef arg1y
#undef arg1z

#undef arg2s
#undef arg2t
#undef arg2u
#undef arg2v
#undef arg2w
#undef arg2x
#undef arg2y
#undef arg2z
	
#undef ress
#undef rest
#undef resu
#undef resv
#undef resw
#undef resx
#undef resy
#undef resz

#undef rems
#undef remt
#undef remu
#undef remv
#undef remw
#undef remx
#undef remy
#undef remz

#undef tmp_var
#undef tmp_var1
#undef tmp_var2
#undef tmp_var3
#undef tmp_var4
#undef tmp_var5
#undef tmp_var6
#undef cnt

#endif /* defined (L_divmoddi4) */

#if defined (L_udivmoddi4)		
#define	arg1s 1(SP)
#define arg1t 2(SP)
#define arg1u 3(SP)
#define arg1v 4(SP)
#define arg1w 5(SP)
#define arg1x 6(SP)
#define arg1y 7(SP)
#define arg1z 8(SP)

#define arg2s 9(SP)
#define arg2t 10(SP)
#define arg2u 11(SP)
#define arg2v 12(SP)
#define arg2w 13(SP)
#define arg2x 14(SP)
#define arg2y 15(SP)
#define arg2z 16(SP)
	
#define ress $80
#define rest $81
#define resu $82
#define resv $83
#define resw $84
#define resx $85
#define resy $86
#define resz $87

#define rems $88
#define remt $89
#define remu $8a
#define remv $8b
#define remw $8c
#define remx $8d
#define remy $8e
#define remz $8f

#define tmp_var	$90
#define tmp_var1 $91
#define tmp_var2 $92
#define tmp_var3 $93
#define tmp_var4 $94
#define tmp_var5 $95
#define tmp_var6 $96
#define cnt $97

	.sect	.text.libgcc,"ax"
	.global	__udivmoddi4
	.func	_udivmoddi4, __udivmoddi4

__udivmoddi4:
	clr	rems
	clr	remt
	clr	remu
	clr	remv
	clr	remw
	clr	remx
	clr	remy
	clr	remz
	mov	w, #65
	mov	cnt, w
	clrb	status, 0
	page	1f
	jmp	1f

2:	rl	remz
	rl	remy
	rl	remx
	rl	remw
	rl	remv
	rl	remu
	rl	remt
	rl	rems
	mov	w, arg2z
	sub	w, remz
	mov	tmp_var, w
	mov	w, arg2y
	subc	w, remy
	mov	tmp_var1, w
	mov	w, arg2x
	subc	w, remx
	mov	tmp_var2, w
	mov	w, arg2w
	subc	w, remw
	mov	tmp_var3, w
	mov	w, arg2v
	subc	w, remv
	mov	tmp_var4, w
	mov	w, arg2u
	subc	w, remu
	mov	tmp_var5, w
	mov	w, arg2t
	subc	w, remt
	mov	tmp_var6, w
	mov	w, arg2s
	subc	w, rems
	sc
	page	1f
	jmp	1f

	mov	rems, w
	mov	w, tmp_var6
	mov	remt, w
	mov	w, tmp_var5
	mov	remu, w
	mov	w, tmp_var4
	mov	remv, w
	mov	w, tmp_var3
	mov	remw, w
	mov	w, tmp_var2
	mov	remx, w
	mov	w, tmp_var1
	mov	remy, w
	mov	w, tmp_var
	mov	remz, w

1:	rl	arg1z
	rl	arg1y
	rl	arg1x
	rl	arg1w
	rl	arg1v
	rl	arg1u
	rl	arg1t
	rl	arg1s
	decsz	cnt
	page	2b
	jmp	2b

	pop	ress
	pop	rest
	pop	resu
	pop	resv
	pop	resw
	pop	resx
	pop	resy
	pop	resz
	mov	w, #8
	add	spl, w
	ret

	.endfunc

#undef arg1s
#undef arg1t
#undef arg1u
#undef arg1v
#undef arg1w
#undef arg1x
#undef arg1y
#undef arg1z

#undef arg2s
#undef arg2t
#undef arg2u
#undef arg2v
#undef arg2w
#undef arg2x
#undef arg2y
#undef arg2z
	
#undef ress
#undef rest
#undef resu
#undef resv
#undef resw
#undef resx
#undef resy
#undef resz

#undef rems
#undef remt
#undef remu
#undef remv
#undef remw
#undef remx
#undef remy
#undef remz

#undef tmp_var
#undef tmp_var1
#undef tmp_var2
#undef tmp_var3
#undef tmp_var4
#undef tmp_var5
#undef tmp_var6
#undef cnt

#endif /* defined (L_udivmoddi4) */

#define LT	#0
#define EQ	#1
#define GT	#2

#if defined(L_cmphi2)
#define arg1l 2(sp)
#define arg1h 1(sp)
#define arg2l 4(sp)
#define arg2h 3(sp)

	.sect	.text.libgcc,"ax"
	.global	__cmphi2
	.global	__cmp_ret
	.global	__cmpqi_ret
	.func	_cmphi2, __cmphi2

__cmphi2:
	mov	w,arg1l	
	sub	w,arg2l
	snz
	page	2f
	jmp	2f
	mov	w,arg1h
1:
	subc	w,arg2h
	clr	arg2l
	rl	arg2l
	snb	arg1h,7
	setb	arg2l,2
	snb	arg2h,7
	setb	arg2l,1
	mov	w,#3

__cmp_ret:
	add	spl,w			;  sign1
	pop	wreg			;   sign2

__cmpqi_ret:
	add	pcl,w			;    carry of arg1 - arg2
	retw	GT			; [000] arg1 > arg2
	retw	LT			; [001] arg1 < arg2
	retw	GT			; [010] arg1 > arg2
	retw	GT			; [011] arg1 > arg2
	retw	LT			; [100] arg1 < arg2
	retw	LT			; [101] arg1 < arg2
	retw	GT			; [110] arg1 > arg2
	retw	LT			; [111] arg1 < arg2
2:
	mov	w,arg1h
	cse	w,arg2h
	page	1b
	jmp	1b
	mov	w,#4
	add	spl,w
	retw	EQ

	.endfunc
#undef arg1l
#undef arg1h
#undef arg2l
#undef arg2h
#endif  /* L_cmphi2 */

#if defined(L_cmpqi2)
#define arg1 1(sp)
#define arg2 2(sp)

	.sect	.text.libgcc,"ax"
	.global	__cmpqi2
	.func	_cmpqi2, __cmpqi2

__cmpqi2:
	mov	w, arg1
	sub	w, arg2
	snz
	page	2f
	jmp	2f

	clr	wreg
	rl	wreg
	snb	arg1, 7
	setb	wreg, 2
	snb	arg2, 7
	setb	wreg, 1
	inc	spl
	inc	spl
	page	__cmpqi_ret
	jmp	__cmpqi_ret

2:	mov	w, #2
	add	spl, w
	retw	EQ

	.endfunc
#undef arg1l
#undef arg2l
#endif  /* L_cmpqi2 */

#if defined(L_cmpsi2)
#define arg1d 4(sp)
#define arg1c 3(sp)
#define arg1b 2(sp)
#define arg1a 1(sp)
#define arg2d 8(sp)
#define arg2c 7(sp)
#define arg2b 6(sp)
#define arg2a 5(sp)
	
	.sect	.text.libgcc,"ax"
	.global	__cmpsi2
	.func	_cmpsi2, __cmpsi2

__cmpsi2:
	mov	w, arg1d
	sub	w, arg2d
	snz
	page	2f
	jmp	2f

1:	mov	w, arg1c
	subc	w, arg2c
	mov	w, arg1b
	subc	w, arg2b
	mov	w, arg1a
	subc	w, arg2a

	clr	arg2d
	rl	arg2d
	snb	arg1a, 7
	setb	arg2d, 2
	snb	arg2a, 7
	setb	arg2d, 1
	mov	w, #7
	page	__cmp_ret
	jmp	__cmp_ret

2:	mov	w, arg1c
	cse	w, arg2c
	page	1b
	jmp	1b

	mov	w, arg1b
	cse	w, arg2b
	page	1b
	jmp	1b

	mov	w, arg1a
	cse	w, arg2a
	page	1b
	jmp	1b

	mov	w, #8
	add	spl, w
	retw	EQ

	.endfunc

#undef arg1d
#undef arg1c
#undef arg1b
#undef arg1a
#undef arg2d
#undef arg2c
#undef arg2b
#undef arg2a
#endif  /* L_cmpsi2 */

#if defined(L_cmpdi2)
#define arg1z 8(sp)
#define arg1y 7(sp)
#define arg1x 6(sp)
#define arg1w 5(sp)
#define arg1v 4(sp)
#define arg1u 3(sp)
#define arg1t 2(sp)
#define arg1s 1(sp)

#define arg2z 16(sp)
#define arg2y 15(sp)
#define arg2x 14(sp)
#define arg2w 13(sp)
#define arg2v 12(sp)
#define arg2u 11(sp)
#define arg2t 10(sp)
#define arg2s 9(sp)
	
	.sect	.text.libgcc,"ax"
	.global	__cmpdi2
	.func	_cmpdi2, __cmpdi2

__cmpdi2:
	mov	w, arg1z
	sub	w, arg2z
	snz
	page	2f
	jmp	2f

1:	mov	w, arg1y
	subc	w, arg2y
	mov	w, arg1x
	subc	w, arg2x
	mov	w, arg1w
	subc	w, arg2w
	mov	w, arg1v
	subc	w, arg2v
	mov	w, arg1u
	subc	w, arg2u
	mov	w, arg1t
	subc	w, arg2t
	mov	w, arg1s
	subc	w, arg2s
	clr	arg2z
	rl	arg2z
	snb	arg1s, 7
	setb	arg2z, 2
	snb	arg2s, 7
	setb	arg2z, 1
	mov	w, #15
	page	__cmp_ret
	jmp	__cmp_ret

2:	mov	w, arg1y
	cse	w, arg2y
	page	1b
	jmp	1b

	mov	w, arg1x
	cse	w, arg2x
	page	1b
	jmp	1b

	mov	w, arg1w
	cse	w, arg2w
	page	1b
	jmp	1b

	mov	w, arg1v
	cse	w, arg2v
	page	1b
	jmp	1b

	mov	w, arg1u
	cse	w, arg2u
	page	1b
	jmp	1b

	mov	w, arg1t
	cse	w, arg2t
	page	1b
	jmp	1b

	mov	w, arg1s
	cse	w, arg2s
	page	1b
	jmp	1b

	mov	w, #16
	add	spl, w
	retw	EQ

	.endfunc
	
#undef arg1z
#undef arg1y
#undef arg1x
#undef arg1w
#undef arg1v
#undef arg1u
#undef arg1t
#undef arg1s
#undef arg2z
#undef arg2y
#undef arg2x
#undef arg2w
#undef arg2v
#undef arg2u
#undef arg2t
#undef arg2s

#endif  /* L_cmpdi2 */
	
#if defined(L_cmpdi2_dp)
	.sect	.text.libgcc,"ax"
	.global	__cmpdi2_dp
	.func	_cmpdi2_dp, __cmpdi2_dp

__cmpdi2_dp:
	push	7(dp)
	push	6(dp)
	push	5(dp)
	push	4(dp)
	push	3(dp)
	push	2(dp)
	push	1(dp)
	push	(dp)
	page	__cmpdi2
	jmp	__cmpdi2

	.endfunc
#endif  /* L_cmpdi2_dp */

#if defined(L_fp_pop_args_ret)
	.sect	.pram.libgcc,"ax"
	.global	__fp_pop_args_ret
	.global	__pop_args_ret
	.global	__pop2_args_ret
	.func	__fp_pop2_args_ret, __fp_pop2_args_ret

__fp_pop2_args_ret:
	mov	w, #2
__fp_pop_args_ret:
	pop	0xfd
	pop	0xfe
__pop_args_ret:
	pop	callh
	pop	calll
	add	spl, w
	ret

	.endfunc
#endif /* L_fp_pop_args_ret */

#if defined(L__pop2_args_ret)
	.sect	.pram.libgcc,"ax"
	.global	__pop2_args_ret
	.func	__pop2_args_ret, __pop2_args_ret

__pop2_args_ret:
	mov	w, #2
	pop	callh
	pop	calll
	add	spl, w
	ret

	.endfunc
#endif /* L__pop2_args_ret */

#if defined(L_leaf_fp_pop_args_ret)
	.sect	.pram.libgcc,"ax"
	.global	__leaf_fp_pop_args_ret, __leaf_fp_pop2_args_ret
	.func	__leaf_fp2_pop_args_ret, __leaf_fp_pop2_args_ret

__leaf_fp_pop2_args_ret:
	mov	w, #2
__leaf_fp_pop_args_ret:
	pop	0xfd
	pop	0xfe
	add	spl, w
	ret

	.endfunc
#endif /* L_leaf_fp_pop_args_ret */

#if defined(L_movstrhi_countqi)
	.sect	.pram.libgcc,"ax"
	.global	__movstrhi_countqi
	.func	_movstrhi_countqi, __movstrhi_countqi

__movstrhi_countqi:
	push	dph			; Save our pointer regs
	push	dpl
	push	iph
	push	ipl

	mov	w, 5(SP)		; Get our dest pointer
	mov	dph, w
	mov	w, 6(SP)
	mov	dpl, w
	mov	w, 7(SP)		; And our source pointer
	mov	iph, w
	mov	w, 8(SP)
	mov	ipl, w

1:	push	(IP)			; *dest++ = *src++
	pop	0(DP)
	inc	ipl
	inc	dpl
	decsz	9(SP)			; Loop until completed
	page	1b
	jmp	1b

	pop	ipl			; Restore our pointer regs
	pop	iph
	pop	dpl
	pop	dph

	mov	w, #5			; Tidy up our stack args
	add	spl, w
	ret

	.endfunc
#endif

#if defined(L_movstrhi_counthi)
	.sect	.text.libgcc,"ax"
	.global	__movstrhi_counthi
	.func	_movstrhi_counthi, __movstrhi_counthi

__movstrhi_counthi:
	push	dph			; Save our pointer regs
	push	dpl
	push	iph
	push	ipl

	mov	w, 5(SP)		; Get our dest pointer
	mov	dph, w
	mov	w, 6(SP)
	mov	dpl, w
	mov	w, 7(SP)		; And our source pointer
	mov	iph, w
	mov	w, 8(SP)
	mov	ipl, w

	test	10(SP)			; If we have a nonzero LSB then adjust the
	sz				; MSB of the loop count to allow us to use
	inc	9(SP)			; skip tricks!

1:	push	(IP)			; *dest++ = *src++
	pop	0(DP)
	inc	ipl
	inc	dpl
	decsnz	10(SP)			; Loop until completed - note the skip trick
	decsz	9(SP)			; on the MSB!
	page	1b
	jmp	1b

	pop	ipl			; Restore our pointer regs
	pop	iph
	pop	dpl
	pop	dph

	mov	w, #6			; Tidy up our stacked args.
	add	spl, w
	ret

	.endfunc
#endif

#if defined(L_exit)
	.sect	.text.libgcc,"ax"
	.global	__exit
	.global	_exit
	.func	_exit, __exit
	.weak	__exit
	.weak	_exit

_exit:		
__exit:
	pop	$88
	pop	wreg
	or	w, $88
	push    wreg
	push	#0
	push	#1
	system				; Exit wreg
	page	__exit			; Never return
	jmp	__exit

	.endfunc
#endif

#if defined(Labort)
	.sect	.text.libgcc,"ax"
	.global	_abort
	.func	abort, _abort

_abort:
	push	#1
	push	#0
	push	#1
	system				; Exit 1
	ret

	.endfunc
#endif

#if defined(Lwrite)
	/* Dummy entrypoint to suppress problems with glue code. */
	.sect	.text.libgcc,"ax"
	.global	_write
	.func	write, _write
;;
;; write (fil,buf,len) - say that write succeeds....
;; 
_write:
	movb	$80, 5(SP)
	movb	$81, 6(SP)		; Return length written
	mov	w, #6
	add	spl, w
	ret
	
	.endfunc
#endif
