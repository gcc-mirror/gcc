/* Miscellaneous BPABI functions.

   Copyright (C) 2003, 2004, 2007, 2008, 2009  Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifdef L_aeabi_lcmp

ARM_FUNC_START aeabi_lcmp
	cmp	xxh, yyh
	do_it	lt
	movlt	r0, #-1
	do_it	gt
	movgt	r0, #1
	do_it	ne
	RETc(ne)
	subs	r0, xxl, yyl
	do_it	lo
	movlo	r0, #-1
	do_it	hi
	movhi	r0, #1
	RET
	FUNC_END aeabi_lcmp

#endif /* L_aeabi_lcmp */
	
#ifdef L_aeabi_ulcmp

ARM_FUNC_START aeabi_ulcmp
	cmp	xxh, yyh
	do_it	lo
	movlo	r0, #-1
	do_it	hi
	movhi	r0, #1
	do_it	ne
	RETc(ne)
	cmp	xxl, yyl
	do_it	lo
	movlo	r0, #-1
	do_it	hi
	movhi	r0, #1
	do_it	eq
	moveq	r0, #0
	RET
	FUNC_END aeabi_ulcmp

#endif /* L_aeabi_ulcmp */

#ifdef L_aeabi_ldivmod

ARM_FUNC_START aeabi_ldivmod
	sub sp, sp, #8
#if defined(__thumb2__)
	mov ip, sp
	push {ip, lr}
#else
	do_push {sp, lr}
#endif
	bl SYM(__gnu_ldivmod_helper) __PLT__
	ldr lr, [sp, #4]
	add sp, sp, #8
	do_pop {r2, r3}
	RET
	
#endif /* L_aeabi_ldivmod */

#ifdef L_aeabi_uldivmod

ARM_FUNC_START aeabi_uldivmod
	sub sp, sp, #8
#if defined(__thumb2__)
	mov ip, sp
	push {ip, lr}
#else
	do_push {sp, lr}
#endif
	bl SYM(__gnu_uldivmod_helper) __PLT__
	ldr lr, [sp, #4]
	add sp, sp, #8
	do_pop {r2, r3}
	RET
	
#endif /* L_aeabi_divmod */
	
