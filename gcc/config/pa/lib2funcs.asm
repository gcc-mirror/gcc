;  Subroutines for calling unbound dynamic functions from within GDB for HPPA.
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

	.SPACE $PRIVATE$
	.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31
	.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=82
	.SPACE $TEXT$
	.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44
	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	.SUBSPA $MILLICODE$,QUAD=0,ALIGN=8,ACCESS=44,SORT=8

	.IMPORT $$dyncall,MILLICODE
; gcc_compiled.:
	.SPACE $TEXT$
	.SUBSPA $CODE$

; Simply call with the address of the desired import stub in %r22 and
; arguments in the normal place (%r26-%r23 and stack slots).
;
	.align 4
	.EXPORT __gcc_plt_call,ENTRY,PRIV_LEV=3,RTNVAL=GR
__gcc_plt_call
	.PROC
	.CALLINFO
	.ENTRY
	; Our return address comes in %r31, not %r2!
	stw %r31,-8(%r30)

	; An inline version of dyncall so we don't have to worry
	; about long calls to millicode, PIC and other complexities.
	bb,>=,n %r22,30,L$foo
        depi 0,31,2,%r22
        ldw 4(%r22),%r19
        ldw 0(%r22),%r22
L$foo
        ldsid (%r22),%r1
        mtsp %r1,%sr0
        ble 0(%sr0,%r22)
	copy %r31,%r2
	ldw -8(%r30),%r2

	; We're going to be returning to a stack address, so we
	; need to do an intra-space return.
	ldsid (%rp),%r1
	mtsp %r1,%sr0
	be,n 0(%sr0,%rp)
	.EXIT
	.PROCEND
