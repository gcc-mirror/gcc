/* Copyright (C) 2000 Free Software Foundation, Inc.
   This file was pretty much copied from newlib.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it
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

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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

#if defined (__SH3E__) || defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__)
	mov.l set_fpscr_k, r1
	jsr @r1
	mov #0,r4
	lds r3,fpscr
#endif /*  defined (__SH3E__) || defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__) */

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
#if defined (__SH3E__) || defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__)
set_fpscr_k:
	.long	___set_fpscr
#endif /*  defined (__SH3E__) || defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__) */
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

#ifdef __ELF__
	.section .stack,"aw"
#else
	.section .stack
#endif
_stack:	.long	0xdeaddead
