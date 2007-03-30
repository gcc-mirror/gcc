/* stuff needed for libgcc on win32.
 *
 *   Copyright (C) 1996, 1998, 2001, 2003 Free Software Foundation, Inc.
 *   Written By Steve Chamberlain
 * 
 * This file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 * 
 * In addition to the permissions in the GNU General Public License, the
 * Free Software Foundation gives you unlimited permission to link the
 * compiled version of this file with other programs, and to distribute
 * those programs without any restriction coming from the use of this
 * file.  (The General Public License restrictions do apply in other
 * respects; for example, they cover modification of the file, and
 * distribution when not linked into another program.)
 * 
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 * 
 *    As a special exception, if you link this library with files
 *    compiled with GCC to produce an executable, this does not cause
 *    the resulting executable to be covered by the GNU General Public License.
 *    This exception does not however invalidate any other reasons why
 *    the executable file might be covered by the GNU General Public License.
 */

#ifdef L_chkstk

/* Function prologue calls _alloca to probe the stack when allocating more
   than CHECK_STACK_LIMIT bytes in one go.  Touching the stack at 4K
   increments is necessary to ensure that the guard pages used
   by the OS virtual memory manger are allocated in correct sequence.  */

	.global ___chkstk
	.global	__alloca
#ifndef _WIN64
___chkstk:
__alloca:
	pushl	%ecx		/* save temp */
	leal	8(%esp), %ecx	/* point past return addr */
	cmpl	$0x1000, %eax	/* > 4k ?*/
	jb	Ldone

Lprobe:
	subl	$0x1000, %ecx  		/* yes, move pointer down 4k*/
	orl	$0x0, (%ecx)   		/* probe there */
	subl	$0x1000, %eax  	 	/* decrement count */
	cmpl	$0x1000, %eax
	ja	Lprobe         	 	/* and do it again */

Ldone:
	subl	%eax, %ecx	   
	orl	$0x0, (%ecx)	/* less than 4k, just peek here */

	movl	%esp, %eax	/* save old stack pointer */
	movl	%ecx, %esp	/* decrement stack */
	movl	(%eax), %ecx	/* recover saved temp */
	movl	4(%eax), %eax	/* recover return address */

	/* Push the return value back.  Doing this instead of just
	   jumping to %eax preserves the cached call-return stack
	   used by most modern processors.  */
	pushl	%eax
	ret
#else
/* __alloca is a normal function call, which uses %rcx as the argument.  */
__alloca:
	movq	%rcx, %rax
	/* FALLTHRU */

/* ___chkstk is a *special* function call, which uses %rax as the argument.
   We avoid clobbering the 4 integer argument registers, %rcx, %rdx, 
   %r8 and %r9, which leaves us with %rax, %r10, and %r11 to use.  */
___chkstk:
	popq	%r11		/* pop return address */
	movq	%rsp, %r10	/* get sp */
	cmpq	$0x1000, %rax	/* > 4k ?*/
	jb	Ldone

Lprobe:
	subq	$0x1000, %r10  		/* yes, move pointer down 4k*/
	orl	$0x0, (%r10)   		/* probe there */
	subq	$0x1000, %rax  	 	/* decrement count */
	cmpq	$0x1000, %rax
	ja	Lprobe         	 	/* and do it again */

Ldone:
	subq	%rax, %r10
	orl	$0x0, (%r10)	/* less than 4k, just peek here */
	movq	%r10, %rsp	/* decrement stack */

	/* Push the return value back.  Doing this instead of just
	   jumping to %r11 preserves the cached call-return stack
	   used by most modern processors.  */
	pushq	%r11
	ret
#endif
#endif
