/* Support functions for the unwinder.
   Copyright (C) 2003, 2004, 2005, 2007  Free Software Foundation, Inc.
   Contributed by Paul Brook

   This file is free software; you can redistribute it and/or modify it
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

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING.  If not, write to
   the Free Software Foundation, 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* An executable stack is *not* required for these functions.  */
#if defined(__ELF__) && defined(__linux__)
.section .note.GNU-stack,"",%progbits
.previous
#endif

#ifndef __symbian__

#include "lib1funcs.asm"

.macro UNPREFIX name
	.global SYM (\name)
	EQUIV SYM (\name), SYM (__\name)
.endm

/* r0 points to a 16-word block.  Upload these values to the actual core
   state.  */
ARM_FUNC_START restore_core_regs
	/* We must use sp as the base register when restoring sp.  Push the
	   last 3 registers onto the top of the current stack to achieve
	   this.  */
	add r1, r0, #52
	ldmia r1, {r3, r4, r5}  /* {sp, lr, pc}.  */
#if defined(__thumb2__)
	/* Thumb-2 doesn't allow sp in a load-multiple instruction, so push
	   the target address onto the target stack.  This is safe as
	   we're always returning to somewhere further up the call stack.  */
	mov ip, r3
	mov lr, r4
	str r5, [ip, #-4]!
#elif defined(__INTERWORKING__)
	/* Restore pc into ip.  */
	mov r2, r5
	stmfd sp!, {r2, r3, r4}
#else
	stmfd sp!, {r3, r4, r5}
#endif
	/* Don't bother restoring ip.  */
	ldmia r0, {r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, sl, fp}
#if defined(__thumb2__)
	/* Pop the return address off the target stack.  */
	mov sp, ip
	pop {pc}
#elif defined(__INTERWORKING__)
	/* Pop the three registers we pushed earlier.  */
	ldmfd sp, {ip, sp, lr}
	bx ip
#else
	ldmfd sp, {sp, lr, pc}
#endif
	FUNC_END restore_core_regs
	UNPREFIX restore_core_regs

/* Load VFP registers d0-d15 from the address in r0.
   Use this to load from FSTMX format.  */
ARM_FUNC_START gnu_Unwind_Restore_VFP
	/* Use the generic coprocessor form so that gas doesn't complain
	   on soft-float targets.  */
	ldc   p11,cr0,[r0],{0x21} /* fldmiax r0, {d0-d15} */
	RET

/* Store VFP registers d0-d15 to the address in r0.
   Use this to store in FSTMX format.  */
ARM_FUNC_START gnu_Unwind_Save_VFP
	/* Use the generic coprocessor form so that gas doesn't complain
	   on soft-float targets.  */
	stc   p11,cr0,[r0],{0x21} /* fstmiax r0, {d0-d15} */
	RET

/* Load VFP registers d0-d15 from the address in r0.
   Use this to load from FSTMD format.  */
ARM_FUNC_START gnu_Unwind_Restore_VFP_D
	ldc   p11,cr0,[r0],{0x20} /* fldmiad r0, {d0-d15} */
	RET

/* Store VFP registers d0-d15 to the address in r0.
   Use this to store in FLDMD format.  */
ARM_FUNC_START gnu_Unwind_Save_VFP_D
	stc   p11,cr0,[r0],{0x20} /* fstmiad r0, {d0-d15} */
	RET

/* Load VFP registers d16-d31 from the address in r0.
   Use this to load from FSTMD (=VSTM) format.  Needs VFPv3.  */
ARM_FUNC_START gnu_Unwind_Restore_VFP_D_16_to_31
	ldcl  p11,cr0,[r0],{0x20} /* vldm r0, {d16-d31} */
	RET

/* Store VFP registers d16-d31 to the address in r0.
   Use this to store in FLDMD (=VLDM) format.  Needs VFPv3.  */
ARM_FUNC_START gnu_Unwind_Save_VFP_D_16_to_31
	stcl  p11,cr0,[r0],{0x20} /* vstm r0, {d16-d31} */
	RET

/* Wrappers to save core registers, then call the real routine.   */

.macro  UNWIND_WRAPPER name nargs
	ARM_FUNC_START \name
	/* Create a phase2_vrs structure.  */
	/* Split reg push in two to ensure the correct value for sp.  */
#if defined(__thumb2__)
	mov ip, sp
	push {lr} /* PC is ignored.  */
	push {ip, lr} /* Push original SP and LR.  */
#else
	stmfd sp!, {sp, lr, pc}
#endif
	stmfd sp!, {r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, sl, fp, ip}
	
	/* Demand-save flags, plus an extra word for alignment.  */
	mov r3, #0
	stmfd sp!, {r2, r3}

	/* Point r1 at the block.  Pass r[0..nargs) unchanged.  */
	add r\nargs, sp, #4
#if defined(__thumb__) && !defined(__thumb2__)
	/* Switch back to thumb mode to avoid interworking hassle.  */
	adr ip, .L1_\name
	orr ip, ip, #1
	bx ip
	.thumb
.L1_\name:
	bl SYM (__gnu\name) __PLT__
	ldr r3, [sp, #64]
	add sp, #72
	bx r3
#else
	bl SYM (__gnu\name) __PLT__
	ldr lr, [sp, #64]
	add sp, sp, #72
	RET
#endif
	FUNC_END \name
	UNPREFIX \name
.endm

UNWIND_WRAPPER _Unwind_RaiseException 1
UNWIND_WRAPPER _Unwind_Resume 1
UNWIND_WRAPPER _Unwind_Resume_or_Rethrow 1
UNWIND_WRAPPER _Unwind_ForcedUnwind 3

#endif  /* __symbian__ */
