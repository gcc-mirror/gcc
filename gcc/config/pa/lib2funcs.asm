;  Subroutines for calling unbound dynamic functions from within GDB for HPPA.
;  Copyright (C) 1994 Free Software Foundation, Inc.

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
;  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

	.SPACE $PRIVATE$
	.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31
	.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=82
	.SPACE $TEXT$
	.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44
	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
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
	.CALLINFO FRAME=64,CALLS,SAVE_RP
	.ENTRY
	stw %r2,-8(0,%r30)
	bl $$dyncall,%r31
	copy %r31,%r2
	ldw -8(0,%r30),%r2
	bv,n 0(%r2)
	.EXIT
	.PROCEND
