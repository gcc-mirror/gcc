/* Copyright (C) 2000, 2001, 2003 Free Software Foundation, Inc.
   This file was pretty much copied from newlib.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#ifdef __SH5__
	.section .data,"aw"
	.global ___data
___data:

	.section .rodata,"a"
	.global ___rodata
___rodata:

#if __SH5__ == 64
	.section .text,"ax"
#define LOAD_ADDR(sym, reg) \
	movi	(sym >> 48) & 65535, reg; \
	shori	(sym >> 32) & 65535, reg; \
	shori	(sym >> 16) & 65535, reg; \
	shori	sym & 65535, reg
#else
	.mode	SHmedia
	.section .text..SHmedia32,"ax"
#define LOAD_ADDR(sym, reg) \
	movi	(sym >> 16) & 65535, reg; \
	shori	sym & 65535, reg
#endif
	.global start
start:
	LOAD_ADDR (_stack, r15)

	pt/l	.Lzero_bss_loop, tr0
	pt/l	_atexit, tr1
	pt/l	_init, tr5
	pt/l	___setup_argv_and_call_main, tr6
	pt/l	_exit, tr7

	! zero out bss
	LOAD_ADDR (_edata, r0)
	LOAD_ADDR (_end, r1)
.Lzero_bss_loop:
	stx.q	r0, r63, r63
	addi	r0, 8, r0
	bgt/l	r1, r0, tr0

	LOAD_ADDR (___data, r26)
	LOAD_ADDR (___rodata, r27)

#if ! __SH4_NOFPU__ && ! __SH2A_NOFPU__
#if __SH5__ == 32
	pt/l ___set_fpscr, tr0
	movi	0, r4
	blink	tr0, r18
#endif
	getcon	sr, r0
	! enable the FP unit, by resetting SR.FD
	! also zero out SR.FR, SR.SZ and SR.PR, as mandated by the ABI
	movi	0, r1
	shori	0xf000, r1
	andc	r0, r1, r0
	putcon	r0, sr
#endif

	! arrange for exit to call fini
	LOAD_ADDR (_fini, r2)
	blink	tr1, r18

	! call init
	blink	tr5, r18

	! call the mainline
	blink	tr6, r18

	! call exit
	blink	tr7, r18
	
#else
	.section .text
	.global	start
start:
	mov.l	stack_k,r15

	! zero out bss
	mov.l	edata_k,r0
	mov.l	end_k,r1
	mov	#0,r2
start_l:
	mov.l	r2,@r0
	add	#4,r0
	cmp/ge	r0,r1
	bt	start_l

#if ! __SH2A_NOFPU__
#if defined (__SH2E__) || defined (__SH2A__) || defined (__SH3E__) || defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__)
	mov.l set_fpscr_k, r1
	jsr @r1
	mov #0,r4
	lds r3,fpscr
#endif /*  defined (__SH2E__) || defined (__SH2A__) || defined (__SH3E__) || defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__) */
#endif /* ! __SH2A_NOFPU__ */

	! arrange for exit to call fini
	mov.l	atexit_k,r0
	mov.l	fini_k,r4
	jsr	@r0
	nop

	! call init
	mov.l	init_k,r0
	jsr	@r0
	nop

	! call the mainline	
	mov.l	main_k,r0
	jsr	@r0
	nop

	! call exit
	mov	r0,r4
	mov.l	exit_k,r0
	jsr	@r0
	nop

	.align 2
#if ! __SH2A_NOFPU__
#if defined (__SH2E__) || defined (__SH2A__) || defined (__SH3E__) || defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__)
set_fpscr_k:
	.long	___set_fpscr
#endif /*  defined (__SH2E__) || defined (__SH2A__) || defined (__SH3E__) || defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__) */
#endif /* ! __SH2A_NOFPU__ */

stack_k:
	.long	_stack	
edata_k:
	.long	_edata
end_k:
	.long	_end
main_k:
	.long	___setup_argv_and_call_main
exit_k:
	.long	_exit
atexit_k:
	.long	_atexit
init_k:
	.long	_init
fini_k:
	.long	_fini

	! supplied for backward compatibility only, in case of linking
	! code whose main() was compiled with an older version of GCC.
	.global	___main
___main:
	rts
	nop
#endif
