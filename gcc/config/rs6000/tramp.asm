/*  Special support for trampolines
 *
 *   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
 *   Written By Michael Meissner
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
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 * 
 *    As a special exception, if you link this library with files
 *    compiled with GCC to produce an executable, this does not cause
 *    the resulting executable to be covered by the GNU General Public License.
 *    This exception does not however invalidate any other reasons why
 *    the executable file might be covered by the GNU General Public License.
 */ 

/* Set up trampolines */

	.file	"tramp.asm"
	.section ".text"
	#include "ppc-asm.h"

	.globl	__trampoline_initial
	.type	__trampoline_initial,@object
	.align	2
__trampoline_initial:
	mflr	r0
	bl	1f
.Lfunc = .-__trampoline_initial
	.long	0			/* will be replaced with function address */
.Lchain = .-__trampoline_initial
	.long	0			/* will be replaced with static chain */
1:	mflr	r11
	mtlr	r0
	lwz	r0,0(r11)		/* function address */
	lwz	r11,4(r11)		/* static chain */
	mtctr	r0
	bctr

__trampoline_size = .-__trampoline_initial
	.size	__trampoline_initial,__trampoline_size

        .section ".got2","aw"
.LCTOC1 = .+32768
.Ltramp = .-.LCTOC1
        .long __trampoline_initial-4

	.section ".text"
.LCL0:
        .long .LCTOC1-.LCF0

/* R3 = stack address to store trampoline */
/* R4 = length of trampoline area */
/* R5 = function address */
/* R6 = static chain */

FUNC_START(__trampoline_setup)
	mflr	r0			/* save return address */
        bl	.LCF0			/* load up __trampoline_initial into r7 */
.LCF0:
        mflr	r11
        lwz	r12,(.LCL0-.LCF0)(r11)
        add	r11,r12,r11
        lwz	r7,.Ltramp(r11)		/* trampoline address -4 */

	li	r8,__trampoline_size	/* verify that the trampoline is big enough */
	cmpw	cr1,r8,r4
	srwi	r4,r4,2			/* # words to move */
	addi	r9,r3,-4		/* adjust pointer for lwzu */
	mtctr	r4
	blt	cr1,.Labort

	mtlr	r0

	/* Copy the instructions to the stack */
.Lmove:
	lwzu	r10,4(r7)
	stwu	r10,4(r9)
	bdnz	.Lmove

	/* Store correct function and static chain */
	stw	r5,.Lfunc(r3)
	stw	r6,.Lchain(r3)

	/* Now flush both caches */
	mtctr	r4
.Lcache:
	icbi	0,r3
	dcbf	0,r3
	addi	r3,r3,4
	bdnz	.Lcache

	/* Finally synchronize things & return */
	sync
	isync
	blr

.Labort:
	bl	abort
FUNC_END(__trampoline_setup)
/* END CYGNUS LOCAL -- waiting for FSF sources to be restored/meissner */
