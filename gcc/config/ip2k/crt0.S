;
;   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
;   Contributed by Red Hat, Inc.
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
; Boston, MA 02111-1307, USA.
;
	
	.file	"crt0.S"
	.text
	.global	__start
	.func	__start
__start:
	clr	$ff		; Insure we have a zero available
	mov	w,#%hi8data(__stack) ; set up stack
	mov	sph,w		;  
	mov	w,#%lo8data(__stack)
	mov	spl,w

	push	#0		; Set argc/argv.
	push	#0		; Only required for testing
	push	#0		;  purposes and "ansi" main.
	push	#0
	page	_main
	call	_main
	push	$81		; use return value to call exit()
	push	$80
	page	_exit
	call	_exit
	break			; Should never return
	.endfunc
