;  Subroutines for out of line prologues and epilogues on for the HPPA
;  Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.

;  This file is part of GNU CC.

;  GNU CC is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.

;  GNU CC is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.

;  You should have received a copy of the GNU General Public License
;  along with GNU CC; see the file COPYING.  If not, write to
;  the Free Software Foundation, 59 Temple Place - Suite 330,
;  Boston, MA 02111-1307, USA.

	.SPACE $PRIVATE$
	.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31
	.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=82
	.SPACE $TEXT$
	.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44
	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	.SUBSPA $MILLICODE$,QUAD=0,ALIGN=8,ACCESS=44,SORT=8


; This is an out-of-line prologue.
;
; It performs the following operations:
;
;	* Saves the return pointer at sp - 20
;
;	* Creates a new stack frame (sp'), size of the frame is passed in %r21
;
;	* The old stack pointer is saved at sp (frame pointer version only).
;
;	* Saves grs (passed in low 16 bits of %r22 into the stack frame
;	at sp' + local_fsize (passed in %r19).
;
;	* Saves frs (passed in high 16 bits of %r22) into the stack
;	frame at sp' + local_fsize (passed in %r19).
;
;	* Sets up a frame pointer (in %r3) (frame pointer version only).
;
;	* Returns to the instruction _immediately_ after the call to
;	this function.

	.SPACE $TEXT$
	.SUBSPA $MILLICODE$
	.EXPORT __outline_prologue_fp,MILLICODE
	.align 32
__outline_prologue_fp
	.PROC
	.CALLINFO FRAME=0,NO_CALLS
	.ENTRY
	copy %r30,%r20

	; Subtract 4 from our return pointer so that we return to
	; the right location.
        ldo -4(%r31),%r31

	; Save off %r2
	stw %r2,-20(%r30)

	; Make our new frame.
	add %r21,%r30,%r30

	; Save our old stack pointer.
	stw %r20,0(%r20)

	; Add in local_fsize to our frame pointer so we do register
	; saves into the right place
	add %r20,%r19,%r20

	; %r22 tells us what registers we need to save.  The upper half
	; is for fp registers, the lower half for integer registers.
	; We put the lower half in %r1 and the upper half into %r22
	; for later use.
	extru %r22,31,16,%r1
	extrs %r22,15,16,%r22

	; %r1 now olds a value 0-18 which corresponds to the number
	; of grs we need to save.  We need to reverse that value so
	; we can just into the table and straight-line execute to the
	; end of the gr saves.
	comb,= %r0,%r1,L$0002
	subi 18,%r1,%r1
	blr,n %r1,%r0
	b,n L$0002
	stws,ma %r18,4(%r20)
	nop
	stws,ma %r17,4(%r20)
	nop
	stws,ma %r16,4(%r20)
	nop
	stws,ma %r15,4(%r20)
	nop
	stws,ma %r14,4(%r20)
	nop
	stws,ma %r13,4(%r20)
	nop
	stws,ma %r12,4(%r20)
	nop
	stws,ma %r11,4(%r20)
	nop
	stws,ma %r10,4(%r20)
	nop
	stws,ma %r9,4(%r20)
	nop
	stws,ma %r8,4(%r20)
	nop
	stws,ma %r7,4(%r20)
	nop
	stws,ma %r6,4(%r20)
	nop
	stws,ma %r5,4(%r20)
	nop
	stws,ma %r4,4(%r20)
	nop
	stws,ma %r3,4(%r20)
	nop
L$0002
	; All gr saves are done.  Align the temporary frame pointer and
	; do the fr saves.
	ldo 7(%r20),%r20
	depi 0,31,3,%r20

	comb,= %r0,%r22,L$0003
	subi 21,%r22,%r22
	blr,n %r22,%r0
	b,n L$0003
	fstws,ma %fr21,8(%r20)
	nop
	fstws,ma %fr20,8(%r20)
	nop
	fstws,ma %fr19,8(%r20)
	nop
	fstws,ma %fr18,8(%r20)
	nop
	fstws,ma %fr17,8(%r20)
	nop
	fstws,ma %fr16,8(%r20)
	nop
	fstws,ma %fr15,8(%r20)
	nop
	fstws,ma %fr14,8(%r20)
	nop
	fstws,ma %fr13,8(%r20)
	nop
	fstws,ma %fr12,8(%r20)
	nop
L$0003
	; Return, setting up a frame pointer in the delay slot
	bv %r0(%r31)
	sub %r30,%r21,%r3
	.EXIT
	.PROCEND


; This is an out-of-line epilogue.  It's operation is basically the reverse
; of the out-of-line prologue.

	.EXPORT __outline_epilogue_fp,MILLICODE
	.align 32
__outline_epilogue_fp
	.PROC
	.CALLINFO FRAME=0,NO_CALLS
	.ENTRY
	; Make a copy of our frame pointer into %r20
	copy %r3,%r20

	; Subtract 4 from our return pointer so that we return to
	; the right location.
        ldo -4(%r31),%r31

	; Reload %r2
	; First save off %r2
	ldw -20(%r20),%r2

	; Load our old stack pointer, save it in %r21.
	ldw 0(%r20),%r21

	; Add in local_fsize (%r19) to the frame pointer to find
	; the saved registers.
	add %r20,%r19,%r20

	; %r22 tells us what registers we need to restore.  The upper half
	; is for fp registers, the lower half for integer registers.
	; We put the lower half in %r1 and the upper half into %r22
	; for later use.
	extru %r22,31,16,%r1
	extrs %r22,15,16,%r22

	; %r1 now olds a value 0-18 which corresponds to the number
	; of grs we need to restore.  We need to reverse that value so
	; we can just into the table and straight-line execute to the
	; end of the gr restore.
	comb,= %r0,%r1,L$0006
	subi 18,%r1,%r1
	blr,n %r1,%r0
	b,n L$0006
	ldws,ma 4(%r20),%r18
	nop
	ldws,ma 4(%r20),%r17
	nop
	ldws,ma 4(%r20),%r16
	nop
	ldws,ma 4(%r20),%r15
	nop
	ldws,ma 4(%r20),%r14
	nop
	ldws,ma 4(%r20),%r13
	nop
	ldws,ma 4(%r20),%r12
	nop
	ldws,ma 4(%r20),%r11
	nop
	ldws,ma 4(%r20),%r10
	nop
	ldws,ma 4(%r20),%r9
	nop
	ldws,ma 4(%r20),%r8
	nop
	ldws,ma 4(%r20),%r7
	nop
	ldws,ma 4(%r20),%r6
	nop
	ldws,ma 4(%r20),%r5
	nop
	ldws,ma 4(%r20),%r4
	nop
	ldws,ma 4(%r20),%r3
	nop
L$0006
	; All gr restore are done.  Align the temporary frame pointer and
	; do the fr restore.
	ldo 7(%r20),%r20
	depi 0,31,3,%r20

	comb,= %r0,%r22,L$0007
	subi 21,%r22,%r22
	blr,n %r22,%r0
	b,n L$0007
	fldws,ma 8(%r20),%fr21
	nop
	fldws,ma 8(%r20),%fr20
	nop
	fldws,ma 8(%r20),%fr19
	nop
	fldws,ma 8(%r20),%fr18
	nop
	fldws,ma 8(%r20),%fr17
	nop
	fldws,ma 8(%r20),%fr16
	nop
	fldws,ma 8(%r20),%fr15
	nop
	fldws,ma 8(%r20),%fr14
	nop
	fldws,ma 8(%r20),%fr13
	nop
	fldws,ma 8(%r20),%fr12
	nop
L$0007
	; Return and deallocate our frame.
	bv %r0(%r31)
	copy %r21,%r30
	.EXIT
	.PROCEND


