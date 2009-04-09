/*  Special support for trampolines
 *
 *   Copyright (C) 1996, 1997, 2000, 2007, 2008, 2009 Free Software Foundation, Inc.
 *   Written By Michael Meissner
 * 
 * This file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * 
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * Under Section 7 of GPL version 3, you are granted additional
 * permissions described in the GCC Runtime Library Exception, version
 * 3.1, as published by the Free Software Foundation.
 *
 * You should have received a copy of the GNU General Public License and
 * a copy of the GCC Runtime Library Exception along with this program;
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 * <http://www.gnu.org/licenses/>.
 */ 

/* Set up trampolines.  */

	.section ".text"
#include "ppc-asm.h"
#include "config.h"

#ifndef __powerpc64__
	.type	trampoline_initial,@object
	.align	2
trampoline_initial:
	mflr	r0
	bcl	20,31,1f
.Lfunc = .-trampoline_initial
	.long	0			/* will be replaced with function address */
.Lchain = .-trampoline_initial
	.long	0			/* will be replaced with static chain */
1:	mflr	r11
	mtlr	r0
	lwz	r0,0(r11)		/* function address */
	lwz	r11,4(r11)		/* static chain */
	mtctr	r0
	bctr

trampoline_size = .-trampoline_initial
	.size	trampoline_initial,trampoline_size


/* R3 = stack address to store trampoline */
/* R4 = length of trampoline area */
/* R5 = function address */
/* R6 = static chain */

FUNC_START(__trampoline_setup)
	mflr	r0		/* save return address */
        bcl	20,31,.LCF0	/* load up __trampoline_initial into r7 */
.LCF0:
        mflr	r11
        addi	r7,r11,trampoline_initial-4-.LCF0 /* trampoline address -4 */

	li	r8,trampoline_size	/* verify that the trampoline is big enough */
	cmpw	cr1,r8,r4
	srwi	r4,r4,2		/* # words to move */
	addi	r9,r3,-4	/* adjust pointer for lwzu */
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
#if (defined __PIC__ || defined __pic__) && defined HAVE_AS_REL16
	bcl	20,31,1f
1:	mflr	r30
	addis	r30,r30,_GLOBAL_OFFSET_TABLE_-1b@ha
	addi	r30,r30,_GLOBAL_OFFSET_TABLE_-1b@l
#endif
	bl	JUMP_TARGET(abort)
FUNC_END(__trampoline_setup)

#endif
