;  Low level integer divide, multiply, remainder, etc routines for the HPPA.
;  Copyright 1995, 2000, 2001 Free Software Foundation, Inc.

;  This file is part of GNU CC.

;  GNU CC is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.

;  In addition to the permissions in the GNU General Public License, the
;  Free Software Foundation gives you unlimited permission to link the
;  compiled version of this file with other programs, and to distribute
;  those programs without any restriction coming from the use of this
;  file.  (The General Public License restrictions do apply in other
;  respects; for example, they cover modification of the file, and
;  distribution when not linked into another program.)

;  GNU CC is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.

;  You should have received a copy of the GNU General Public License
;  along with GNU CC; see the file COPYING.  If not, write to
;  the Free Software Foundation, 59 Temple Place - Suite 330,
;  Boston, MA 02111-1307, USA.

#ifdef __STDC__
#define CAT(a,b)	a##b
#else
#define CAT(a,b)	a/**/b
#endif

#ifdef ELF

#define SPACE \
! .text! .align 4
#define GSYM(sym) \
! .export sym,millicode!sym:
#define LSYM(sym) \
!CAT(.L,sym:)
#define LREF(sym) CAT(.L,sym)

#else

#define SPACE \
! .space $TEXT$! .subspa $MILLICODE$,quad=0,align=8,access=0x2c,sort=8! .align 4
#define GSYM(sym) \
! .export sym,millicode!sym
#define LSYM(sym) \
!CAT(L$,sym)
#define LREF(sym) CAT(L$,sym)
#endif

#ifdef L_dyncall
SPACE
GSYM($$dyncall)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	bb,>=,n	%r22,30,LREF(1)		; branch if not plabel address
	depi	0,31,2,%r22		; clear the two least significant bits
	ldw	4(%r22),%r19		; load new LTP value
	ldw	0(%r22),%r22		; load address of target
LSYM(1)
#ifdef LINUX
	bv	%r0(%r22)		; branch to the real target
#else
	ldsid	(%sr0,%r22),%r1		; get the "space ident" selected by r22
	mtsp	%r1,%sr0		; move that space identifier into sr0
	be	0(%sr0,%r22)		; branch to the real target
#endif
	stw	%r2,-24(%r30)		; save return address into frame marker
	.exit
	.procend
#endif


#ifdef L_multiply
#define	op0	%r26
#define	op1	%r25
#define res	%r29
#define ret	%r31
#define tmp	%r1

SPACE
GSYM($$mulU)
GSYM($$mulI)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	addi,tr		0,%r0,res	; clear out res, skip next insn
LSYM(loop)
	zdep		op1,26,27,op1	; shift up op1 by 5
LSYM(lo)
	zdep		op0,30,5,tmp	; extract next 5 bits and shift up
	blr		tmp,%r0
	extru		op0,26,27,op0	; shift down op0 by 5
LSYM(0)
	comib,<>	0,op0,LREF(lo)
	zdep		op1,26,27,op1	; shift up op1 by 5
	bv		%r0(ret)
	nop
LSYM(1)
	b		LREF(loop)
	addl		op1,res,res
	nop
	nop
LSYM(2)
	b		LREF(loop)
	sh1addl		op1,res,res
	nop
	nop
LSYM(3)
	sh1addl		op1,op1,tmp	; 3x
	b		LREF(loop)
	addl		tmp,res,res
	nop
LSYM(4)
	b		LREF(loop)
	sh2addl		op1,res,res
	nop
	nop
LSYM(5)
	sh2addl		op1,op1,tmp	; 5x
	b		LREF(loop)
	addl		tmp,res,res
	nop
LSYM(6)
	sh1addl		op1,op1,tmp	; 3x
	b		LREF(loop)
	sh1addl		tmp,res,res
	nop
LSYM(7)
	zdep		op1,28,29,tmp	; 8x
	sub		tmp,op1,tmp	; 7x
	b		LREF(loop)
	addl		tmp,res,res
LSYM(8)
	b		LREF(loop)
	sh3addl		op1,res,res
	nop
	nop
LSYM(9)
	sh3addl		op1,op1,tmp	; 9x
	b		LREF(loop)
	addl		tmp,res,res
	nop
LSYM(10)
	sh2addl		op1,op1,tmp	; 5x
	b		LREF(loop)
	sh1addl		tmp,res,res
	nop
LSYM(11)
	sh2addl		op1,op1,tmp	; 5x
	sh1addl		tmp,op1,tmp	; 11x
	b		LREF(loop)
	addl		tmp,res,res
LSYM(12)
	sh1addl		op1,op1,tmp	; 3x
	b		LREF(loop)
	sh2addl		tmp,res,res
	nop
LSYM(13)
	sh1addl		op1,op1,tmp	; 3x
	sh2addl		tmp,op1,tmp	; 13x
	b		LREF(loop)
	addl		tmp,res,res
LSYM(14)
	zdep		op1,28,29,tmp	; 8x
	sub		tmp,op1,tmp	; 7x
	b		LREF(loop)
	sh1addl		tmp,res,res
LSYM(15)
	zdep		op1,27,28,tmp	; 16x
	sub		tmp,op1,tmp	; 15x
	b		LREF(loop)
	addl		tmp,res,res
LSYM(16)
	zdep		op1,27,28,tmp	; 16x
	b		LREF(loop)
	addl		tmp,res,res
	nop
LSYM(17)
	zdep		op1,27,28,tmp	; 16x
	addl		tmp,op1,tmp	; 17x
	b		LREF(loop)
	addl		tmp,res,res
LSYM(18)
	sh3addl		op1,op1,tmp	; 9x
	b		LREF(loop)
	sh1addl		tmp,res,res
	nop
LSYM(19)
	sh3addl		op1,op1,tmp	; 9x
	sh1addl		tmp,op1,tmp	; 19x
	b		LREF(loop)
	addl		tmp,res,res
LSYM(20)
	sh2addl		op1,op1,tmp	; 5x
	b		LREF(loop)
	sh2addl		tmp,res,res
	nop
LSYM(21)
	sh2addl		op1,op1,tmp	; 5x
	sh2addl		tmp,op1,tmp	; 21x
	b		LREF(loop)
	addl		tmp,res,res
LSYM(22)
	sh2addl		op1,op1,tmp	; 5x
	sh1addl		tmp,op1,tmp	; 11x
	b		LREF(loop)
	sh1addl		tmp,res,res
LSYM(23)
	sh1addl		op1,op1,tmp	; 3x
	sh3addl		tmp,res,res	; += 8x3
	b		LREF(loop)
	sub		res,op1,res	; -= x
LSYM(24)
	sh1addl		op1,op1,tmp	; 3x
	b		LREF(loop)
	sh3addl		tmp,res,res	; += 8x3
	nop
LSYM(25)
	sh2addl		op1,op1,tmp	; 5x
	sh2addl		tmp,tmp,tmp	; 25x
	b		LREF(loop)
	addl		tmp,res,res
LSYM(26)
	sh1addl		op1,op1,tmp	; 3x
	sh2addl		tmp,op1,tmp	; 13x
	b		LREF(loop)
	sh1addl		tmp,res,res	; += 2x13
LSYM(27)
	sh1addl		op1,op1,tmp	; 3x
	sh3addl		tmp,tmp,tmp	; 27x
	b		LREF(loop)
	addl		tmp,res,res
LSYM(28)
	zdep		op1,28,29,tmp	; 8x
	sub		tmp,op1,tmp	; 7x
	b		LREF(loop)
	sh2addl		tmp,res,res	; += 4x7
LSYM(29)
	sh1addl		op1,op1,tmp	; 3x
	sub		res,tmp,res	; -= 3x
	b		LREF(foo)
	zdep		op1,26,27,tmp	; 32x
LSYM(30)
	zdep		op1,27,28,tmp	; 16x
	sub		tmp,op1,tmp	; 15x
	b		LREF(loop)
	sh1addl		tmp,res,res	; += 2x15
LSYM(31)
	zdep		op1,26,27,tmp	; 32x
	sub		tmp,op1,tmp	; 31x
LSYM(foo)
	b		LREF(loop)
	addl		tmp,res,res
	.exit
	.procend
#endif


#ifdef L_divU
#define dividend %r26
#define divisor %r25
#define tmp %r1
#define quotient %r29
#define ret %r31

SPACE
GSYM($$divU)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	comb,<		divisor,0,LREF(largedivisor)
	 sub		%r0,divisor,%r1		; clear cy as side-effect
	ds		%r0,%r1,%r0
	addc		dividend,dividend,dividend
	ds		%r0,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,quotient
	ds		%r1,divisor,%r1
	bv		%r0(ret)
	addc		quotient,quotient,quotient
LSYM(largedivisor)
	comclr,<<	dividend,divisor,quotient
	ldi		1,quotient
	bv,n		%r0(ret)
	.exit
	.procend
#endif


#ifdef L_remU
#define dividend %r26
#define divisor %r25
#define quotient %r29
#define tmp %r1
#define ret %r31

SPACE
GSYM($$remU)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	comb,<		divisor,0,LREF(largedivisor)
	 sub		%r0,divisor,%r1		; clear cy as side-effect
	ds		%r0,%r1,%r0
	addc		dividend,dividend,dividend
	ds		%r0,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,quotient
	ds		%r1,divisor,%r1
	comclr,>=	%r1,%r0,%r0
	addl		%r1,divisor,%r1
	bv		%r0(ret)
	copy		%r1,quotient
LSYM(largedivisor)
	sub,>>=		dividend,divisor,quotient
	copy		dividend,quotient
	bv,n		%r0(ret)
	.exit
	.procend
#endif


#ifdef L_divI
#define dividend %r26
#define divisor %r25
#define quotient %r29
#define tmp %r1
#define ret %r31

SPACE
GSYM($$divI)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	xor		dividend,divisor,quotient	; result sign
	comclr,>=	divisor,%r0,%r0			; get absolute values
	sub		%r0,divisor,divisor
	comclr,>=	dividend,%r0,%r0
	sub		%r0,dividend,dividend

	comb,<		divisor,0,LREF(largedivisor)
	 sub		%r0,divisor,%r1		; clear cy as side-effect
	ds		%r0,%r1,%r0
	addc		dividend,dividend,dividend
	ds		%r0,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	comclr,>=	%r1,%r0,%r0
	addl		%r1,divisor,%r1
	comclr,>=	quotient,%r0,%r0	; skip of no need to negate
	sub		%r0,dividend,dividend
	bv		%r0(ret)
	copy		dividend,quotient
LSYM(largedivisor)
	comclr,<<	dividend,divisor,quotient
	ldi		1,quotient
	bv,n		%r0(ret)
	.exit
	.procend
#endif


#ifdef L_remI
#define dividend %r26
#define divisor %r25
#define quotient %r29
#define tmp %r1
#define ret %r31

SPACE
GSYM($$remI)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	xor		dividend,%r0,quotient		; result sign
	comclr,>=	divisor,%r0,%r0			; get absolute values
	sub		%r0,divisor,divisor
	comclr,>=	dividend,%r0,%r0
	sub		%r0,dividend,dividend

	comb,<		divisor,0,LREF(largedivisor)
	 sub		%r0,divisor,%r1		; clear cy as side-effect
	ds		%r0,%r1,%r0
	addc		dividend,dividend,dividend
	ds		%r0,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	ds		%r1,divisor,%r1
	addc		dividend,dividend,dividend
	comclr,>=	%r1,%r0,%r0
	addl		%r1,divisor,%r1
	comclr,>=	quotient,%r0,%r0	; skip of no need to negate
	sub		%r0,%r1,%r1
	bv		%r0(ret)
	copy		%r1,quotient
LSYM(largedivisor)
	sub,>>=		dividend,divisor,quotient
	copy		dividend,quotient
	bv,n		%r0(ret)
	.exit
	.procend
#endif


#if defined (L_divU_3) && !defined (SMALL_LIB)
#undef L_divU_3
#define dividend %r26
#define divisor %r25
#define tmp %r1
#define result %r29
#define ret %r31

SPACE
GSYM($$divU_3)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	sh2add	%r26,%r26,%r29		; r29 = lo(101 x r)
	shd	%r0,%r26,30,%r1		;  r1 = hi(100 x r)
	addc	%r1,%r0,%r1		;  r1 = hi(101 x r)
; r in r1,,r29
	zdep	%r29,27,28,%r25		; r25 = lo(10000 x r)
	add	%r25,%r29,%r25		; r25 = lo(10001 x r)
	shd	%r1,%r29,28,%r29	; r29 = hi(10000 x r)
	addc	%r29,%r1,%r29		; r29 = hi(10001 x r)
; r in r29,,r25
	zdep	%r25,23,24,%r1		;  r1 = lo(100000000 x r)
	add	%r1,%r25,%r1		;  r1 = lo(100000001 x r)
	shd	%r29,%r25,24,%r25	; r25 = hi(100000000 x r)
	addc	%r25,%r29,%r25		; r25 = hi(100000001 x r)
; r in r25,,r1
	zdep	%r1,15,16,%r29
	add	%r29,%r1,%r29
	shd	%r25,%r1,16,%r1
	addc	%r1,%r25,%r1
; r in r1,,r29
	sh1add	%r29,%r26,%r0		;  r0 = lo(10 x r) + dividend
	shd	%r1,%r29,31,%r29	; r29 = hi(10 x r)
	addc	%r29,%r0,%r29
	bv	%r0(ret)
	extru	%r29,30,31,result
	.exit
	.procend
#endif


#if defined (L_divU_5) && !defined (SMALL_LIB)
#undef L_divU_5
#define dividend %r26
#define divisor %r25
#define tmp %r1
#define result %r29
#define ret %r31

SPACE
GSYM($$divU_5)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	sh1add	%r26,%r26,%r29		; r29 = lo(11 x r)
	shd	%r0,%r26,31,%r1		;  r1 = hi(10 x r)
	addc	%r1,%r0,%r1		;  r1 = hi(11 x r)
; r in r1,,r29
	zdep	%r29,27,28,%r25		; r25 = lo(10000 x r)
	add	%r25,%r29,%r25		; r25 = lo(10001 x r)
	shd	%r1,%r29,28,%r29	; r29 = hi(10000 x r)
	addc	%r29,%r1,%r29		; r29 = hi(10001 x r)
; r in r29,,r25
	zdep	%r25,23,24,%r1		;  r1 = lo(100000000 x r)
	add	%r1,%r25,%r1		;  r1 = lo(100000001 x r)
	shd	%r29,%r25,24,%r25	; r25 = hi(100000000 x r)
	addc	%r25,%r29,%r25		; r25 = hi(100000001 x r)
; r in r25,,r1
	zdep	%r1,15,16,%r29
	add	%r29,%r1,%r29
	shd	%r25,%r1,16,%r1
	addc	%r1,%r25,%r1
; r in r1,,r29
	sh2add	%r29,%r26,%r0		;  r0 = lo(1000 x r) + dividend
	shd	%r1,%r29,30,%r29	; r29 = hi(1000 x r)
	addc	%r29,%r0,%r29
	bv	%r0(ret)
	extru	%r29,29,30,result
	.exit
	.procend
#endif


#if defined (L_divU_6) && !defined (SMALL_LIB)
#undef L_divU_6
#define dividend %r26
#define divisor %r25
#define tmp %r1
#define result %r29
#define ret %r31

SPACE
GSYM($$divU_6)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	sh2add	%r26,%r26,%r29		; r29 = lo(101 x r)
	shd	%r0,%r26,30,%r1		;  r1 = hi(100 x r)
	addc	%r1,%r0,%r1		;  r1 = hi(101 x r)
; r in r1,,r29
	zdep	%r29,27,28,%r25		; r25 = lo(10000 x r)
	add	%r25,%r29,%r25		; r25 = lo(10001 x r)
	shd	%r1,%r29,28,%r29	; r29 = hi(10000 x r)
	addc	%r29,%r1,%r29		; r29 = hi(10001 x r)
; r in r29,,r25
	zdep	%r25,23,24,%r1		;  r1 = lo(100000000 x r)
	add	%r1,%r25,%r1		;  r1 = lo(100000001 x r)
	shd	%r29,%r25,24,%r25	; r25 = hi(100000000 x r)
	addc	%r25,%r29,%r25		; r25 = hi(100000001 x r)
; r in r25,,r1
	zdep	%r1,15,16,%r29
	add	%r29,%r1,%r29
	shd	%r25,%r1,16,%r1
	addc	%r1,%r25,%r1
; r in r1,,r29
	sh1add	%r29,%r26,%r0		;  r0 = lo(10 x r) + dividend
	shd	%r1,%r29,31,%r29	; r29 = hi(10 x r)
	addc	%r29,%r0,%r29
	bv	%r0(ret)
	extru	%r29,29,30,result
	.exit
	.procend
#endif


#if defined (L_divU_9) && !defined (SMALL_LIB)
#undef L_divU_9
#define dividend %r26
#define divisor %r25
#define tmp %r1
#define result %r29
#define ret %r31

SPACE
GSYM($$divU_9)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	zdep	%r26,28,29,%r29
	sub	%r29,%r26,%r29
	shd	0,%r26,29,%r1
	subb	%r1,0,%r1		/* 111 */

	zdep	%r29,25,26,%r25
	add	%r25,%r29,%r25
	shd	%r1,%r29,26,%r29
	addc	%r29,%r1,%r29		/* 111000111 */

	sh3add	%r25,%r26,%r1
	shd	%r29,%r25,29,%r25
	addc	%r25,0,%r25		/* 111000111001 */

	zdep	%r1,16,17,%r29
	sub	%r29,%r1,%r29
	shd	%r25,%r1,17,%r1
	subb	%r1,%r25,%r1		/* 111000111000111000111000111 */

	sh3add	%r29,%r26,%r0
	shd	%r1,%r29,29,%r29
	addc	%r29,0,%r29		/* 111000111000111000111000111001 */
	bv	%r0(ret)
	extru	%r29,30,31,result
	.exit
	.procend
#endif


#if defined (L_divU_10) && !defined (SMALL_LIB)
#undef L_divU_10
#define dividend %r26
#define divisor %r25
#define tmp %r1
#define result %r29
#define ret %r31

SPACE
GSYM($$divU_10)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	sh1add	%r26,%r26,%r29		; r29 = lo(11 x r)
	shd	%r0,%r26,31,%r1		;  r1 = hi(10 x r)
	addc	%r1,%r0,%r1		;  r1 = hi(11 x r)
; r in r1,,r29
	zdep	%r29,27,28,%r25		; r25 = lo(10000 x r)
	add	%r25,%r29,%r25		; r25 = lo(10001 x r)
	shd	%r1,%r29,28,%r29	; r29 = hi(10000 x r)
	addc	%r29,%r1,%r29		; r29 = hi(10001 x r)
; r in r29,,r25
	zdep	%r25,23,24,%r1		;  r1 = lo(100000000 x r)
	add	%r1,%r25,%r1		;  r1 = lo(100000001 x r)
	shd	%r29,%r25,24,%r25	; r25 = hi(100000000 x r)
	addc	%r25,%r29,%r25		; r25 = hi(100000001 x r)
; r in r25,,r1
	zdep	%r1,15,16,%r29
	add	%r29,%r1,%r29
	shd	%r25,%r1,16,%r1
	addc	%r1,%r25,%r1
; r in r1,,r29
	sh2add	%r29,%r26,%r0		;  r0 = lo(1000 x r) + dividend
	shd	%r1,%r29,30,%r29	; r29 = hi(1000 x r)
	addc	%r29,%r0,%r29
	bv	%r0(ret)
	extru	%r29,28,29,result
	.exit
	.procend
#endif


#if defined (L_divU_12) && !defined (SMALL_LIB)
#undef L_divU_12
#define dividend %r26
#define divisor %r25
#define tmp %r1
#define result %r29
#define ret %r31

SPACE
GSYM($$divU_12)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	sh2add	%r26,%r26,%r29		; r29 = lo(101 x r)
	shd	%r0,%r26,30,%r1		;  r1 = hi(100 x r)
	addc	%r1,%r0,%r1		;  r1 = hi(101 x r)
; r in r1,,r29
	zdep	%r29,27,28,%r25		; r25 = lo(10000 x r)
	add	%r25,%r29,%r25		; r25 = lo(10001 x r)
	shd	%r1,%r29,28,%r29	; r29 = hi(10000 x r)
	addc	%r29,%r1,%r29		; r29 = hi(10001 x r)
; r in r29,,r25
	zdep	%r25,23,24,%r1		;  r1 = lo(100000000 x r)
	add	%r1,%r25,%r1		;  r1 = lo(100000001 x r)
	shd	%r29,%r25,24,%r25	; r25 = hi(100000000 x r)
	addc	%r25,%r29,%r25		; r25 = hi(100000001 x r)
; r in r25,,r1
	zdep	%r1,15,16,%r29
	add	%r29,%r1,%r29
	shd	%r25,%r1,16,%r1
	addc	%r1,%r25,%r1
; r in r1,,r29
	sh1add	%r29,%r26,%r0		;  r0 = lo(10 x r) + dividend
	shd	%r1,%r29,31,%r29	; r29 = hi(10 x r)
	addc	%r29,%r0,%r29
	bv	%r0(ret)
	extru	%r29,28,29,result
	.exit
	.procend
#endif


#ifdef L_divU_3
SPACE
GSYM($$divU_3)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divU
	ldi		3,%r25
	.exit
	.procend
	.import		$$divU,MILLICODE
#endif

#ifdef L_divU_5
SPACE
GSYM($$divU_5)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divU
	ldi		5,%r25
	.exit
	.procend
	.import		$$divU,MILLICODE
#endif

#ifdef L_divU_6
SPACE
GSYM($$divU_6)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divU
	ldi		6,%r25
	.exit
	.procend
	.import		$$divU,MILLICODE
#endif

#ifdef L_divU_7
SPACE
GSYM($$divU_7)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divU
	ldi		7,%r25
	.exit
	.procend
	.import		$$divU,MILLICODE
#endif

#ifdef L_divU_9
SPACE
GSYM($$divU_9)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divU
	ldi		9,%r25
	.exit
	.procend
	.import		$$divU,MILLICODE
#endif

#ifdef L_divU_10
SPACE
GSYM($$divU_10)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divU
	ldi		10,%r25
	.exit
	.procend
	.import		$$divU,MILLICODE
#endif

#ifdef L_divU_12
SPACE
GSYM($$divU_12)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divU
	ldi		12,%r25
	.exit
	.procend
	.import		$$divU,MILLICODE
#endif

#ifdef L_divU_14
SPACE
GSYM($$divU_14)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divU
	ldi		14,%r25
	.exit
	.procend
	.import		$$divU,MILLICODE
#endif

#ifdef L_divU_15
SPACE
GSYM($$divU_15)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divU
	ldi		15,%r25
	.exit
	.procend
	.import		$$divU,MILLICODE
#endif

#ifdef L_divI_3
SPACE
GSYM($$divI_3)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divI
	ldi		3,%r25
	.exit
	.procend
	.import		$$divI,MILLICODE
#endif

#ifdef L_divI_5
SPACE
GSYM($$divI_5)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divI
	ldi		5,%r25
	.exit
	.procend
	.import		$$divI,MILLICODE
#endif

#ifdef L_divI_6
SPACE
GSYM($$divI_6)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divI
	ldi		6,%r25
	.exit
	.procend
	.import		$$divI,MILLICODE
#endif

#ifdef L_divI_7
SPACE
GSYM($$divI_7)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divI
	ldi		7,%r25
	.exit
	.procend
	.import		$$divI,MILLICODE
#endif

#ifdef L_divI_9
SPACE
GSYM($$divI_9)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divI
	ldi		9,%r25
	.exit
	.procend
	.import		$$divI,MILLICODE
#endif

#ifdef L_divI_10
SPACE
GSYM($$divI_10)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divI
	ldi		10,%r25
	.exit
	.procend
	.import		$$divI,MILLICODE
#endif

#ifdef L_divI_12
SPACE
GSYM($$divI_12)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divI
	ldi		12,%r25
	.exit
	.procend
	.import		$$divI,MILLICODE
#endif

#ifdef L_divI_14
SPACE
GSYM($$divI_14)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divI
	ldi		14,%r25
	.exit
	.procend
	.import		$$divI,MILLICODE
#endif

#ifdef L_divI_15
SPACE
GSYM($$divI_15)
	.proc
	.callinfo	frame=0,no_calls
	.entry
	b		$$divI
	ldi		15,%r25
	.exit
	.procend
	.import		$$divI,MILLICODE
#endif
