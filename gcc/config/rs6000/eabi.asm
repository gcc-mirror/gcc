/*
 * Special support for eabi and SVR4
 *
 *   Copyright (C) 1995, 1996, 1998, 2000, 2001 Free Software Foundation, Inc.
 *   Written By Michael Meissner
 *   64-bit support written by David Edelsohn
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

/* Do any initializations needed for the eabi environment */

	.file	"eabi.asm"
	.section ".text"
	#include "ppc-asm.h"

#ifndef __powerpc64__

	 .section ".got2","aw"
	.align	2
.LCTOC1 = . /* +32768 */

/* Table of addresses */
.Ltable = .-.LCTOC1
	.long	.LCTOC1				/* address we are really at */

.Lsda = .-.LCTOC1
	.long	_SDA_BASE_			/* address of the first small data area */

.Lsdas = .-.LCTOC1
	.long	__SDATA_START__			/* start of .sdata/.sbss section */

.Lsdae = .-.LCTOC1
	.long	__SBSS_END__			/* end of .sdata/.sbss section */

.Lsda2 = .-.LCTOC1
	.long	_SDA2_BASE_			/* address of the second small data area */

.Lsda2s = .-.LCTOC1
	.long	__SDATA2_START__		/* start of .sdata2/.sbss2 section */

.Lsda2e = .-.LCTOC1
	.long	__SBSS2_END__			/* end of .sdata2/.sbss2 section */

#ifdef _RELOCATABLE
.Lgots = .-.LCTOC1
	.long	__GOT_START__			/* Global offset table start */

.Lgotm1 = .-.LCTOC1
	.long	_GLOBAL_OFFSET_TABLE_-4		/* end of GOT ptrs before BLCL + 3 reserved words */

.Lgotm2 = .-.LCTOC1
	.long	_GLOBAL_OFFSET_TABLE_+12	/* start of GOT ptrs after BLCL + 3 reserved words */

.Lgote = .-.LCTOC1
	.long	__GOT_END__			/* Global offset table end */

.Lgot2s = .-.LCTOC1
	.long	__GOT2_START__			/* -mrelocatable GOT pointers start */

.Lgot2e = .-.LCTOC1
	.long	__GOT2_END__			/* -mrelocatable GOT pointers end */

.Lfixups = .-.LCTOC1
	.long	__FIXUP_START__			/* start of .fixup section */

.Lfixupe = .-.LCTOC1
	.long	__FIXUP_END__			/* end of .fixup section */

.Lctors = .-.LCTOC1
	.long	__CTOR_LIST__			/* start of .ctor section */

.Lctore = .-.LCTOC1
	.long	__CTOR_END__			/* end of .ctor section */

.Ldtors = .-.LCTOC1
	.long	__DTOR_LIST__			/* start of .dtor section */

.Ldtore = .-.LCTOC1
	.long	__DTOR_END__			/* end of .dtor section */

.Lexcepts = .-.LCTOC1
	.long	__EXCEPT_START__		/* start of .gcc_except_table section */

.Lexcepte = .-.LCTOC1
	.long	__EXCEPT_END__			/* end of .gcc_except_table section */

.Linit = .-.LCTOC1
	.long	.Linit_p			/* address of variable to say we've been called */

	.text
	.align	2
.Lptr:
	.long	.LCTOC1-.Laddr			/* PC relative pointer to .got2 */
#endif

	.data
	.align	2
.Linit_p:
	.long	0

	.text

FUNC_START(__eabi)

/* Eliminate -mrelocatable code if not -mrelocatable, so that this file can
   be assembled with other assemblers than GAS.  */

#ifndef _RELOCATABLE
	addis	10,0,.Linit_p@ha		/* init flag */
	addis	11,0,.LCTOC1@ha			/* load address of .LCTOC1 */
	lwz	9,.Linit_p@l(10)		/* init flag */
	addi	11,11,.LCTOC1@l
	cmplwi	2,9,0				/* init flag != 0? */
	bnelr	2				/* return now, if we've been called already */
	stw	1,.Linit_p@l(10)		/* store a non-zero value in the done flag */

#else /* -mrelocatable */
	mflr	0
	bl	.Laddr				/* get current address */
.Laddr:
	mflr	12				/* real address of .Laddr */
	lwz	11,(.Lptr-.Laddr)(12)		/* linker generated address of .LCTOC1 */
	add	11,11,12			/* correct to real pointer */
	lwz	12,.Ltable(11)			/* get linker's idea of where .Laddr is */
	lwz	10,.Linit(11)			/* address of init flag */
	subf.	12,12,11			/* calculate difference */
	lwzx	9,10,12				/* done flag */
	cmplwi	2,9,0				/* init flag != 0? */
	mtlr	0				/* restore in case branch was taken */
	bnelr	2				/* return now, if we've been called already */
	stwx	1,10,12				/* store a non-zero value in the done flag */
	beq+	0,.Lsdata			/* skip if we don't need to relocate */

/* We need to relocate the .got2 pointers.  */

	lwz	3,.Lgot2s(11)			/* GOT2 pointers start */
	lwz	4,.Lgot2e(11)			/* GOT2 pointers end */
	add	3,12,3				/* adjust pointers */
	add	4,12,4
	bl	FUNC_NAME(__eabi_convert)	/* convert pointers in .got2 section */

/* Fixup the .ctor section for static constructors */

	lwz	3,.Lctors(11)			/* constructors pointers start */
	lwz	4,.Lctore(11)			/* constructors pointers end */
	bl	FUNC_NAME(__eabi_convert)	/* convert constructors */

/* Fixup the .dtor section for static destructors */

	lwz	3,.Ldtors(11)			/* destructors pointers start */
	lwz	4,.Ldtore(11)			/* destructors pointers end */
	bl	FUNC_NAME(__eabi_convert)	/* convert destructors */

/* Fixup the .gcc_except_table section for G++ exceptions */

	lwz	3,.Lexcepts(11)			/* exception table pointers start */
	lwz	4,.Lexcepte(11)			/* exception table pointers end */
	bl	FUNC_NAME(__eabi_convert)	/* convert exceptions */

/* Fixup the addresses in the GOT below _GLOBAL_OFFSET_TABLE_-4 */

	lwz	3,.Lgots(11)			/* GOT table pointers start */
	lwz	4,.Lgotm1(11)			/* GOT table pointers below _GLOBAL_OFFSET_TABLE-4 */
	bl	FUNC_NAME(__eabi_convert)	/* convert lower GOT */

/* Fixup the addresses in the GOT above _GLOBAL_OFFSET_TABLE_+12 */

	lwz	3,.Lgotm2(11)			/* GOT table pointers above _GLOBAL_OFFSET_TABLE+12 */
	lwz	4,.Lgote(11)			/* GOT table pointers end */
	bl	FUNC_NAME(__eabi_convert)	/* convert lower GOT */

/* Fixup any user initialized pointers now (the compiler drops pointers to */
/* each of the relocs that it does in the .fixup section).  */

.Lfix:
	lwz	3,.Lfixups(11)			/* fixup pointers start */
	lwz	4,.Lfixupe(11)			/* fixup pointers end */
	bl	FUNC_NAME(__eabi_uconvert)	/* convert user initialized pointers */

.Lsdata:
	mtlr	0				/* restore link register */
#endif /* _RELOCATABLE */

/* Only load up register 13 if there is a .sdata and/or .sbss section */
	lwz	3,.Lsdas(11)			/* start of .sdata/.sbss section */
	lwz	4,.Lsdae(11)			/* end of .sdata/.sbss section */
	cmpw	1,3,4				/* .sdata/.sbss section non-empty? */
	beq-	1,.Lsda2l			/* skip loading r13 */

	lwz	13,.Lsda(11)			/* load r13 with _SDA_BASE_ address */

/* Only load up register 2 if there is a .sdata2 and/or .sbss2 section */

.Lsda2l:	
	lwz	3,.Lsda2s(11)			/* start of .sdata/.sbss section */
	lwz	4,.Lsda2e(11)			/* end of .sdata/.sbss section */
	cmpw	1,3,4				/* .sdata/.sbss section non-empty? */
	beq+	1,.Ldone			/* skip loading r2 */

	lwz	2,.Lsda2(11)			/* load r2 with _SDA2_BASE_ address */

/* Done adjusting pointers, return by way of doing the C++ global constructors.  */

.Ldone:
	b	FUNC_NAME(__init)	/* do any C++ global constructors (which returns to caller) */
FUNC_END(__eabi)

/* Special subroutine to convert a bunch of pointers directly.
   r0		has original link register
   r3		has low pointer to convert
   r4		has high pointer to convert
   r5 .. r10	are scratch registers
   r11		has the address of .LCTOC1 in it.
   r12		has the value to add to each pointer
   r13 .. r31	are unchanged */
	
FUNC_START(__eabi_convert)
        cmplw	1,3,4				/* any pointers to convert? */
        subf	5,3,4				/* calculate number of words to convert */
        bclr	4,4				/* return if no pointers */

        srawi	5,5,2
	addi	3,3,-4				/* start-4 for use with lwzu */
        mtctr	5

.Lcvt:
	lwzu	6,4(3)				/* pointer to convert */
	cmpi	0,6,0
	beq-	.Lcvt2				/* if pointer is null, don't convert */

        add	6,6,12				/* convert pointer */
        stw	6,0(3)
.Lcvt2:
        bdnz+	.Lcvt
        blr

FUNC_END(__eabi_convert)

/* Special subroutine to convert the pointers the user has initialized.  The
   compiler has placed the address of the initialized pointer into the .fixup
   section.

   r0		has original link register
   r3		has low pointer to convert
   r4		has high pointer to convert
   r5 .. r10	are scratch registers
   r11		has the address of .LCTOC1 in it.
   r12		has the value to add to each pointer
   r13 .. r31	are unchanged */
	
FUNC_START(__eabi_uconvert)
        cmplw	1,3,4				/* any pointers to convert? */
        subf	5,3,4				/* calculate number of words to convert */
        bclr	4,4				/* return if no pointers */

        srawi	5,5,2
	addi	3,3,-4				/* start-4 for use with lwzu */
        mtctr	5

.Lucvt:
	lwzu	6,4(3)				/* next pointer to pointer to convert */
	add	6,6,12				/* adjust pointer */
	lwz	7,0(6)				/* get the pointer it points to */
	stw	6,0(3)				/* store adjusted pointer */
	add	7,7,12				/* adjust */
	stw	7,0(6)
        bdnz+	.Lucvt
        blr

FUNC_END(__eabi_uconvert)

/* Routines for saving floating point registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the floating point save area.  */

FUNC_START(_savefpr_14)	stfd	14,-144(11)	/* save fp registers */
FUNC_START(_savefpr_15)	stfd	15,-136(11)
FUNC_START(_savefpr_16)	stfd	16,-128(11)
FUNC_START(_savefpr_17)	stfd	17,-120(11)
FUNC_START(_savefpr_18)	stfd	18,-112(11)
FUNC_START(_savefpr_19)	stfd	19,-104(11)
FUNC_START(_savefpr_20)	stfd	20,-96(11)
FUNC_START(_savefpr_21)	stfd	21,-88(11)
FUNC_START(_savefpr_22)	stfd	22,-80(11)
FUNC_START(_savefpr_23)	stfd	23,-72(11)
FUNC_START(_savefpr_24)	stfd	24,-64(11)
FUNC_START(_savefpr_25)	stfd	25,-56(11)
FUNC_START(_savefpr_26)	stfd	26,-48(11)
FUNC_START(_savefpr_27)	stfd	27,-40(11)
FUNC_START(_savefpr_28)	stfd	28,-32(11)
FUNC_START(_savefpr_29)	stfd	29,-24(11)
FUNC_START(_savefpr_30)	stfd	30,-16(11)
FUNC_START(_savefpr_31)	stfd	31,-8(11)
			blr
FUNC_END(_savefpr_31)
FUNC_END(_savefpr_30)
FUNC_END(_savefpr_29)
FUNC_END(_savefpr_28)
FUNC_END(_savefpr_27)
FUNC_END(_savefpr_26)
FUNC_END(_savefpr_25)
FUNC_END(_savefpr_24)
FUNC_END(_savefpr_23)
FUNC_END(_savefpr_22)
FUNC_END(_savefpr_21)
FUNC_END(_savefpr_20)
FUNC_END(_savefpr_19)
FUNC_END(_savefpr_18)
FUNC_END(_savefpr_17)
FUNC_END(_savefpr_16)
FUNC_END(_savefpr_15)
FUNC_END(_savefpr_14)

/* Routines for saving integer registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the integer save area.  */

FUNC_START(_savegpr_14)	stw	14,-72(11)	/* save gp registers */
FUNC_START(_savegpr_15)	stw	15,-68(11)
FUNC_START(_savegpr_16)	stw	16,-64(11)
FUNC_START(_savegpr_17)	stw	17,-60(11)
FUNC_START(_savegpr_18)	stw	18,-56(11)
FUNC_START(_savegpr_19)	stw	19,-52(11)
FUNC_START(_savegpr_20)	stw	20,-48(11)
FUNC_START(_savegpr_21)	stw	21,-44(11)
FUNC_START(_savegpr_22)	stw	22,-40(11)
FUNC_START(_savegpr_23)	stw	23,-36(11)
FUNC_START(_savegpr_24)	stw	24,-32(11)
FUNC_START(_savegpr_25)	stw	25,-28(11)
FUNC_START(_savegpr_26)	stw	26,-24(11)
FUNC_START(_savegpr_27)	stw	27,-20(11)
FUNC_START(_savegpr_28)	stw	28,-16(11)
FUNC_START(_savegpr_29)	stw	29,-12(11)
FUNC_START(_savegpr_30)	stw	30,-8(11)
FUNC_START(_savegpr_31)	stw	31,-4(11)
			blr
FUNC_END(_savegpr_31)
FUNC_END(_savegpr_30)
FUNC_END(_savegpr_29)
FUNC_END(_savegpr_28)
FUNC_END(_savegpr_27)
FUNC_END(_savegpr_26)
FUNC_END(_savegpr_25)
FUNC_END(_savegpr_24)
FUNC_END(_savegpr_23)
FUNC_END(_savegpr_22)
FUNC_END(_savegpr_21)
FUNC_END(_savegpr_20)
FUNC_END(_savegpr_19)
FUNC_END(_savegpr_18)
FUNC_END(_savegpr_17)
FUNC_END(_savegpr_16)
FUNC_END(_savegpr_15)
FUNC_END(_savegpr_14)

/* Routines for restoring floating point registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the floating point save area.  */

FUNC_START(_restfpr_14)	lfd	14,-144(11)	/* restore fp registers */
FUNC_START(_restfpr_15)	lfd	15,-136(11)
FUNC_START(_restfpr_16)	lfd	16,-128(11)
FUNC_START(_restfpr_17)	lfd	17,-120(11)
FUNC_START(_restfpr_18)	lfd	18,-112(11)
FUNC_START(_restfpr_19)	lfd	19,-104(11)
FUNC_START(_restfpr_20)	lfd	20,-96(11)
FUNC_START(_restfpr_21)	lfd	21,-88(11)
FUNC_START(_restfpr_22)	lfd	22,-80(11)
FUNC_START(_restfpr_23)	lfd	23,-72(11)
FUNC_START(_restfpr_24)	lfd	24,-64(11)
FUNC_START(_restfpr_25)	lfd	25,-56(11)
FUNC_START(_restfpr_26)	lfd	26,-48(11)
FUNC_START(_restfpr_27)	lfd	27,-40(11)
FUNC_START(_restfpr_28)	lfd	28,-32(11)
FUNC_START(_restfpr_29)	lfd	29,-24(11)
FUNC_START(_restfpr_30)	lfd	30,-16(11)
FUNC_START(_restfpr_31)	lfd	31,-8(11)
			blr
FUNC_END(_restfpr_31)
FUNC_END(_restfpr_30)
FUNC_END(_restfpr_29)
FUNC_END(_restfpr_28)
FUNC_END(_restfpr_27)
FUNC_END(_restfpr_26)
FUNC_END(_restfpr_25)
FUNC_END(_restfpr_24)
FUNC_END(_restfpr_23)
FUNC_END(_restfpr_22)
FUNC_END(_restfpr_21)
FUNC_END(_restfpr_20)
FUNC_END(_restfpr_19)
FUNC_END(_restfpr_18)
FUNC_END(_restfpr_17)
FUNC_END(_restfpr_16)
FUNC_END(_restfpr_15)
FUNC_END(_restfpr_14)

/* Routines for restoring integer registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the integer restore area.  */

FUNC_START(_restgpr_14)	lwz	14,-72(11)	/* restore gp registers */
FUNC_START(_restgpr_15)	lwz	15,-68(11)
FUNC_START(_restgpr_16)	lwz	16,-64(11)
FUNC_START(_restgpr_17)	lwz	17,-60(11)
FUNC_START(_restgpr_18)	lwz	18,-56(11)
FUNC_START(_restgpr_19)	lwz	19,-52(11)
FUNC_START(_restgpr_20)	lwz	20,-48(11)
FUNC_START(_restgpr_21)	lwz	21,-44(11)
FUNC_START(_restgpr_22)	lwz	22,-40(11)
FUNC_START(_restgpr_23)	lwz	23,-36(11)
FUNC_START(_restgpr_24)	lwz	24,-32(11)
FUNC_START(_restgpr_25)	lwz	25,-28(11)
FUNC_START(_restgpr_26)	lwz	26,-24(11)
FUNC_START(_restgpr_27)	lwz	27,-20(11)
FUNC_START(_restgpr_28)	lwz	28,-16(11)
FUNC_START(_restgpr_29)	lwz	29,-12(11)
FUNC_START(_restgpr_30)	lwz	30,-8(11)
FUNC_START(_restgpr_31)	lwz	31,-4(11)
			blr
FUNC_END(_restgpr_31)
FUNC_END(_restgpr_30)
FUNC_END(_restgpr_29)
FUNC_END(_restgpr_28)
FUNC_END(_restgpr_27)
FUNC_END(_restgpr_26)
FUNC_END(_restgpr_25)
FUNC_END(_restgpr_24)
FUNC_END(_restgpr_23)
FUNC_END(_restgpr_22)
FUNC_END(_restgpr_21)
FUNC_END(_restgpr_20)
FUNC_END(_restgpr_19)
FUNC_END(_restgpr_18)
FUNC_END(_restgpr_17)
FUNC_END(_restgpr_16)
FUNC_END(_restgpr_15)
FUNC_END(_restgpr_14)

/* Routines for restoring floating point registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the floating point save area.  */
/* In addition to restoring the fp registers, it will return to the caller's */
/* caller */

FUNC_START(_restfpr_14_x)	lfd	14,-144(11)	/* restore fp registers */
FUNC_START(_restfpr_15_x)	lfd	15,-136(11)
FUNC_START(_restfpr_16_x)	lfd	16,-128(11)
FUNC_START(_restfpr_17_x)	lfd	17,-120(11)
FUNC_START(_restfpr_18_x)	lfd	18,-112(11)
FUNC_START(_restfpr_19_x)	lfd	19,-104(11)
FUNC_START(_restfpr_20_x)	lfd	20,-96(11)
FUNC_START(_restfpr_21_x)	lfd	21,-88(11)
FUNC_START(_restfpr_22_x)	lfd	22,-80(11)
FUNC_START(_restfpr_23_x)	lfd	23,-72(11)
FUNC_START(_restfpr_24_x)	lfd	24,-64(11)
FUNC_START(_restfpr_25_x)	lfd	25,-56(11)
FUNC_START(_restfpr_26_x)	lfd	26,-48(11)
FUNC_START(_restfpr_27_x)	lfd	27,-40(11)
FUNC_START(_restfpr_28_x)	lfd	28,-32(11)
FUNC_START(_restfpr_29_x)	lfd	29,-24(11)
FUNC_START(_restfpr_30_x)	lfd	30,-16(11)
FUNC_START(_restfpr_31_x)	lwz	0,4(11)
				lfd	31,-8(11)
				mtlr	0
				mr	1,11
				blr
FUNC_END(_restfpr_31_x)
FUNC_END(_restfpr_30_x)
FUNC_END(_restfpr_29_x)
FUNC_END(_restfpr_28_x)
FUNC_END(_restfpr_27_x)
FUNC_END(_restfpr_26_x)
FUNC_END(_restfpr_25_x)
FUNC_END(_restfpr_24_x)
FUNC_END(_restfpr_23_x)
FUNC_END(_restfpr_22_x)
FUNC_END(_restfpr_21_x)
FUNC_END(_restfpr_20_x)
FUNC_END(_restfpr_19_x)
FUNC_END(_restfpr_18_x)
FUNC_END(_restfpr_17_x)
FUNC_END(_restfpr_16_x)
FUNC_END(_restfpr_15_x)
FUNC_END(_restfpr_14_x)

/* Routines for restoring integer registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the integer restore area.  */

FUNC_START(_restgpr_14_x)	lwz	14,-72(11)	/* restore gp registers */
FUNC_START(_restgpr_15_x)	lwz	15,-68(11)
FUNC_START(_restgpr_16_x)	lwz	16,-64(11)
FUNC_START(_restgpr_17_x)	lwz	17,-60(11)
FUNC_START(_restgpr_18_x)	lwz	18,-56(11)
FUNC_START(_restgpr_19_x)	lwz	19,-52(11)
FUNC_START(_restgpr_20_x)	lwz	20,-48(11)
FUNC_START(_restgpr_21_x)	lwz	21,-44(11)
FUNC_START(_restgpr_22_x)	lwz	22,-40(11)
FUNC_START(_restgpr_23_x)	lwz	23,-36(11)
FUNC_START(_restgpr_24_x)	lwz	24,-32(11)
FUNC_START(_restgpr_25_x)	lwz	25,-28(11)
FUNC_START(_restgpr_26_x)	lwz	26,-24(11)
FUNC_START(_restgpr_27_x)	lwz	27,-20(11)
FUNC_START(_restgpr_28_x)	lwz	28,-16(11)
FUNC_START(_restgpr_29_x)	lwz	29,-12(11)
FUNC_START(_restgpr_30_x)	lwz	30,-8(11)
FUNC_START(_restgpr_31_x)	lwz	0,4(11)
				lwz	31,-4(11)
				mtlr	0
				mr	1,11
				blr
FUNC_END(_restgpr_31_x)
FUNC_END(_restgpr_30_x)
FUNC_END(_restgpr_29_x)
FUNC_END(_restgpr_28_x)
FUNC_END(_restgpr_27_x)
FUNC_END(_restgpr_26_x)
FUNC_END(_restgpr_25_x)
FUNC_END(_restgpr_24_x)
FUNC_END(_restgpr_23_x)
FUNC_END(_restgpr_22_x)
FUNC_END(_restgpr_21_x)
FUNC_END(_restgpr_20_x)
FUNC_END(_restgpr_19_x)
FUNC_END(_restgpr_18_x)
FUNC_END(_restgpr_17_x)
FUNC_END(_restgpr_16_x)
FUNC_END(_restgpr_15_x)
FUNC_END(_restgpr_14_x)

#else /* __powerpc64__ */

	.section ".text"
	.align 2

/* Routines for saving floating point registers, called by the compiler.  */

.fsav:
FUNC_START(_savef14)	stfd	14,-144(1)	/* save fp registers */
FUNC_START(_savef15)	stfd	15,-136(1)
FUNC_START(_savef16)	stfd	16,-128(1)
FUNC_START(_savef17)	stfd	17,-120(1)
FUNC_START(_savef18)	stfd	18,-112(1)
FUNC_START(_savef19)	stfd	19,-104(1)
FUNC_START(_savef20)	stfd	20,-96(1)
FUNC_START(_savef21)	stfd	21,-88(1)
FUNC_START(_savef22)	stfd	22,-80(1)
FUNC_START(_savef23)	stfd	23,-72(1)
FUNC_START(_savef24)	stfd	24,-64(1)
FUNC_START(_savef25)	stfd	25,-56(1)
FUNC_START(_savef26)	stfd	26,-48(1)
FUNC_START(_savef27)	stfd	27,-40(1)
FUNC_START(_savef28)	stfd	28,-32(1)
FUNC_START(_savef29)	stfd	29,-24(1)
FUNC_START(_savef30)	stfd	30,-16(1)
FUNC_START(_savef31)	stfd	31,-8(1)
			blr
.LTfsav:
			.long 0
			.byte 0,12,0,0,0,0,0,0
			.long 0
			.long .LTfsav-.fsav
			.short 4
			.ascii "fsav"
FUNC_END(_savef31)
FUNC_END(_savef30)
FUNC_END(_savef29)
FUNC_END(_savef28)
FUNC_END(_savef27)
FUNC_END(_savef26)
FUNC_END(_savef25)
FUNC_END(_savef24)
FUNC_END(_savef23)
FUNC_END(_savef22)
FUNC_END(_savef21)
FUNC_END(_savef20)
FUNC_END(_savef19)
FUNC_END(_savef18)
FUNC_END(_savef17)
FUNC_END(_savef16)
FUNC_END(_savef15)
FUNC_END(_savef14)

/* Routines for restoring floating point registers, called by the compiler.  */

.fres:
FUNC_START(_restf14)	lfd	14,-144(1)	/* restore fp registers */
FUNC_START(_restf15)	lfd	15,-136(1)
FUNC_START(_restf16)	lfd	16,-128(1)
FUNC_START(_restf17)	lfd	17,-120(1)
FUNC_START(_restf18)	lfd	18,-112(1)
FUNC_START(_restf19)	lfd	19,-104(1)
FUNC_START(_restf20)	lfd	20,-96(1)
FUNC_START(_restf21)	lfd	21,-88(1)
FUNC_START(_restf22)	lfd	22,-80(1)
FUNC_START(_restf23)	lfd	23,-72(1)
FUNC_START(_restf24)	lfd	24,-64(1)
FUNC_START(_restf25)	lfd	25,-56(1)
FUNC_START(_restf26)	lfd	26,-48(1)
FUNC_START(_restf27)	lfd	27,-40(1)
FUNC_START(_restf28)	lfd	28,-32(1)
FUNC_START(_restf29)	lfd	29,-24(1)
FUNC_START(_restf30)	lfd	30,-16(1)
FUNC_START(_restf31)	lfd	31,-8(1)
			blr
.LTfres:
			.long 0
			.byte 0,12,0,0,0,0,0,0
			.long 0
			.long .LTfres-.fres
			.short 4
			.ascii "fres"
FUNC_END(_restf31)
FUNC_END(_restf30)
FUNC_END(_restf29)
FUNC_END(_restf28)
FUNC_END(_restf27)
FUNC_END(_restf26)
FUNC_END(_restf25)
FUNC_END(_restf24)
FUNC_END(_restf23)
FUNC_END(_restf22)
FUNC_END(_restf21)
FUNC_END(_restf20)
FUNC_END(_restf19)
FUNC_END(_restf18)
FUNC_END(_restf17)
FUNC_END(_restf16)
FUNC_END(_restf15)
FUNC_END(_restf14)

#endif
