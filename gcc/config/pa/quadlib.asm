;  Subroutines for long double support on the HPPA
;  Copyright (C) 1999 Free Software Foundation, Inc.

;  This file is part of GNU CC.

;  GNU CC is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.

;  GNU CC is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.

; In addition to the permissions in the GNU General Public License, the
; Free Software Foundation gives you unlimited permission to link the
; compiled version of this file with other programs, and to distribute
; those programs without any restriction coming from the use of this
; file.  (The General Public License restrictions do apply in other
; respects; for example, they cover modification of the file, and
; distribution when not linked into another program.)

;  You should have received a copy of the GNU General Public License
;  along with GNU CC; see the file COPYING.  If not, write to
;  the Free Software Foundation, 59 Temple Place - Suite 330,
;  Boston, MA 02111-1307, USA.

	.SPACE $TEXT$
	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	.compiler "quadlib.asm GNU_PA-RISC_Assembler 2.9.4"
	.IMPORT _U_Qfcmp,CODE
	.IMPORT _U_Qfsub,CODE

	.align 4
	.NSUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	;
	; Check two long doubles for equality
	;
	.EXPORT _U_Qfeq,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,RTNVAL=GR
_U_Qfeq
	.PROC
	.CALLINFO FRAME=64,CALLS,SAVE_RP
	.ENTRY
	;
	; Build the frame
	;
	stw %r2,-20(0,%r30)
	ldo 64(%r30),%r30

	;
	; Load the additional argument and call the comparison routine.
	;
	bl _U_Qfcmp,%r2
	  ldi 4,%r24

	;
	; The return from _U_Qfcmp is the masked C bit from the FP
	; status register. Convert that to a 0 or 1.
	;
	comiclr,= 0,%r28,%r28
	  ldi 1,%r28

	;
	; Return
	;
	ldw -84(0,%r30),%r2
	bv 0(%r2)
	  ldo -64(%r30), %r30

	.EXIT
	.PROCEND

	;
	; Check two long doubles for inequality
	;
	.EXPORT _U_Qfne,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,RTNVAL=GR
_U_Qfne
	.PROC
	.CALLINFO FRAME=64,CALLS,SAVE_RP
	.ENTRY
	;
	; Build the frame
	;
	stw %r2,-20(0,%r30)
	ldo 64(%r30),%r30

	;
	; Load the additional argument and call the comparison routine.
	;
	bl _U_Qfcmp,%r2
	ldi 4,%r24
	
	;
	; The return from _U_Qfcmp is the masked C bit from the FP
	; status register. Convert that to a 0 or 1.
	;
	comiclr,<> 0,%r28,%r28
	  ldi 1,%r28

	;
	; Return
	;
	ldw -84(0,%r30),%r2
	bv 0(%r2)
	  ldo -64(%r30),%r30
	.EXIT
	.PROCEND

	;
	; Check if opnd1 > opnd0                
	;
	.EXPORT _U_Qfgt,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,RTNVAL=GR
_U_Qfgt
	.PROC
	.CALLINFO FRAME=64,CALLS,SAVE_RP
	.ENTRY
	;
	; Build the frame
	;
	stw %r2,-20(0,%r30)
	ldo 64(%r30),%r30

	;
	; Load the additional argument and call the comparison routine.
	;
	bl _U_Qfcmp,%r2
	ldi 17,%r24

	;
	; The return from _U_Qfcmp is the masked C bit from the FP
	; status register. Convert that to a 0 or 1.
	;
	comiclr,= 0,%r28,%r28
	  ldi 1,%r28

	;
	; Return
	;
	ldw -84(0,%r30),%r2
	bv 0(%r2)
	  ldo -64(%r30), %r30

	.EXIT
	.PROCEND

	;
	; Check if opnd1 >= opnd0                
	;
	.EXPORT _U_Qfge,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,RTNVAL=GR
_U_Qfge
	.PROC
	.CALLINFO FRAME=64,CALLS,SAVE_RP
	.ENTRY
	;
	; Build the frame
	;
	stw %r2,-20(0,%r30)
	ldo 64(%r30),%r30

	;
	; Load the additional argument and call the comparison routine.
	;
	bl _U_Qfcmp,%r2
	ldi 21,%r24

	;
	; The return from _U_Qfcmp is the masked C bit from the FP
	; status register. Convert that to a 0 or 1.
	;
	comiclr,= 0,%r28,%r28
	  ldi 1,%r28

	;
	; Return
	;
	ldw -84(0,%r30),%r2
	bv 0(%r2)
	  ldo -64(%r30), %r30

	.EXIT
	.PROCEND

	;
	; Check if opnd1 < opnd0                
	;
	.EXPORT _U_Qflt,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,RTNVAL=GR
_U_Qflt
	.PROC
	.CALLINFO FRAME=64,CALLS,SAVE_RP
	.ENTRY
	;
	; Build the frame
	;
	stw %r2,-20(0,%r30)
	ldo 64(%r30),%r30

	;
	; Load the additional argument and call the comparison routine.
	;
	bl _U_Qfcmp,%r2
	ldi 9,%r24

	;
	; The return from _U_Qfcmp is the masked C bit from the FP
	; status register. Convert that to a 0 or 1.
	;
	comiclr,= 0,%r28,%r28
	  ldi 1,%r28

	;
	; Return
	;
	ldw -84(0,%r30),%r2
	bv 0(%r2)
	  ldo -64(%r30), %r30

	.EXIT
	.PROCEND

	;
	; Check if opnd1 <= opnd0                
	;
	.EXPORT _U_Qfle,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,RTNVAL=GR
_U_Qfle
	.PROC
	.CALLINFO FRAME=64,CALLS,SAVE_RP
	.ENTRY
	;
	; Build the frame
	;
	stw %r2,-20(0,%r30)
	ldo 64(%r30),%r30

	;
	; Load the additional argument and call the comparison routine.
	;
	bl _U_Qfcmp,%r2
	ldi 13,%r24

	;
	; The return from _U_Qfcmp is the masked C bit from the FP
	; status register. Convert that to a 0 or 1.
	;
	comiclr,= 0,%r28,%r28
	  ldi 1,%r28

	;
	; Return
	;
	ldw -84(0,%r30),%r2
	bv 0(%r2)
	  ldo -64(%r30), %r30

	.EXIT
	.PROCEND

	;
	; Negate opnd0 and store in ret0                 
	;
	.EXPORT _U_Qfneg,ENTRY,PRIV_LEV=3,ARGW0=GR,RTNVAL=GR
_U_Qfneg
	.PROC
	.CALLINFO FRAME=128,CALLS,SAVE_RP
	.ENTRY
	;
	; Build the frame
	;
	stw %r2,-20(0,%r30)
	ldo 128(%r30),%r30

	;
	; copy the value to be negated to the frame.
	;
	ldw 0(0,%r26), %r25
	ldw 4(0,%r26), %r24
	ldw 8(0,%r26), %r23
	ldw 12(0,%r26),%r1
	stw %r25, -100(0,%r30)
	stw %r24,  -96(0,%r30)
	stw %r23,  -92(0,%r30)
	stw %r1,   -88(0,%r30)
	ldo -100(%r30), %r25

	;
	; ret0 contains a pointer to the location for the return
	; value. Initialize it to zero and pass it as arg0 to
	; _U_Qfsub.
	;
	copy %r28,%r26
	stw %r0,0(0,%r26)
	stw %r0,4(0,%r26)
	stw %r0,8(0,%r26)
	bl _U_Qfsub,%r2
	  stw %r0,12(0,%r26)

	;
	; Return
	;
	ldw -148(0,%r30),%r2
	bv 0(%r2)
	  ldo -128(%r30), %r30

	.EXIT
	.PROCEND

	;
	; Compare opnd0 and opnd1. If opnd0 == opnd1, return 0.
	; If opnd0 is greater than opnd1, return 1.
	; Otherwise, return -1.
	;
	.EXPORT _U_Qfcomp,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,RTNVAL=GR
_U_Qfcomp
	.PROC
	.CALLINFO FRAME=64,CALLS,SAVE_RP
	.ENTRY
	;
	; Build the frame
	;
	stw %r2,-20(0,%r30)
	ldo 64(%r30),%r30

	;
	; Save arg0 and arg1.
	;
	stw %r26, -60(0,%r30)
	stw %r25, -56(0,%r30)
	;
	; Check for equality
	;
	bl _U_Qfcmp,%r2
	  ldi 4, %r24

	comib,<> 0,%r28,done
	  copy %r0, %r1

	;
	; Reset the parms and test for opnd0 > opnd1.
	;
	ldw -60(0,%r30),%r26
	ldw -56(0,%r30),%r25
	bl _U_Qfcmp,%r2
	  ldi 22,%r24

	ldi 1,%r1
	comiclr,<> 0,%r28,0
	  ldi -1,%r1

done
	copy %r1,%r28
	;
	; Return
	;
	ldw -84(0,%r30),%r2
	bv 0(%r2)
	  ldo -64(%r30), %r30

	.EXIT
	.PROCEND
