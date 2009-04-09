/* Copyright (C) 2000, 2001, 2009 Free Software Foundation, Inc.
   This file was adapted from glibc sources.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* See an explanation about .init and .fini in crti.asm.  */

	.section .init
#if __SHMEDIA__
	add	r14, r63, r15
	ld.q	r15, 0, r18
	ptabs	r18, tr0
	ld.q	r15, 8, r14
	addi	r15, 16, r15
	blink	tr0, r63
#elif __SH5__ && ! __SHMEDIA__
	mov	r14,r15
	lds.l	@r14+,pr
	mov.l	@r14,r14
	rts
	add	#8,r15
#else
	mov	r14,r15
	lds.l	@r15+,pr
	mov.l	@r15+,r14
	rts
#ifdef __ELF__
	mov.l	@r15+,r12
#else
	nop
#endif
#endif /* __SHMEDIA__ */

	.section .fini
#if __SHMEDIA__
	add	r14, r63, r15
	ld.q	r15, 0, r18
	ptabs	r18, tr0
	ld.q	r15, 8, r14
	addi	r15, 16, r15
	blink	tr0, r63
#elif __SH5__ && ! __SHMEDIA__
	mov	r14,r15
	lds.l	@r14+,pr
	mov.l	@r14,r14
	rts
	add	#8,r15
#else
	mov	r14,r15
	lds.l	@r15+,pr
	mov.l	@r15+,r14
	rts
#ifdef __ELF__
	mov.l	@r15+,r12
#else
	nop
#endif
#endif /* __SHMEDIA__ */
