/* libgcc routines for NEC V850.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.

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

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifdef L_mulsi3
	.text
	.globl ___mulsi3
	.type  ___mulsi3,@function

/*
 * #define SHIFT 12
 * #define MASK ((1 << SHIFT) - 1)
 *  
 * #define STEP(i, j)                             \
 * ({                                             \
 *     short a_part = (a >> (i)) & MASK;          \
 *     short b_part = (b >> (j)) & MASK;          \
 *     int res = (((int)a_part) * ((int)b_part)); \
 *     res;                                       \
 * })
 *
 * int
 * __mulsi3 (unsigned a, unsigned b)
 * {
 *    return STEP (0, 0) +
 *        ((STEP (SHIFT, 0) + STEP (0, SHIFT)) << SHIFT) +
 *        ((STEP (0, 2 * SHIFT) + STEP (SHIFT, SHIFT) + STEP (2 * SHIFT, 0))
 *         << (2 * SHIFT));
 * }
 */

___mulsi3:
        mov r6,r13
        movea lo(4095),r0,r16
        and r16,r13
        mov r7,r15
        and r16,r15
        mov r13,r10
        mulh r15,r10
        shr 12,r6
        mov r6,r14
        and r16,r14
        mov r14,r11
        mulh r15,r11
        shr 12,r7
        mov r7,r12
        and r16,r12
        shr 12,r7
        and r16,r7
        mulh r13,r7
        shr 12,r6
        mulh r12,r13
        and r16,r6
        add r13,r11
        shl 12,r11
        add r11,r10
        mov r14,r11
        mulh r12,r11
        mulh r15,r6
        add r11,r7
        add r6,r7
        shl 24,r7
        add r7,r10
        jmp [r31]
	.size ___mulsi3,.-___mulsi3
#endif

#ifdef L_udivsi3
	.text
	.global ___udivsi3
	.type	___udivsi3,@function
___udivsi3:
	mov 1,r12
	mov 0,r10
	cmp r6,r7
	bnl .L12
	movhi hi(-2147483648),r0,r13
	cmp r0,r7
	blt .L12
.L4:
	shl 1,r7
	shl 1,r12
	cmp r6,r7
	bnl .L12
	cmp r0,r12
	be .L8
	mov r7,r19
	and r13,r19
	be .L4
	br .L12
.L9:
	cmp r7,r6
	bl .L10
	sub r7,r6
	or r12,r10
.L10:
	shr 1,r12
	shr 1,r7
.L12:
	cmp r0,r12
	bne .L9
.L8:
	jmp [r31]
	.size ___udivsi3,.-___udivsi3
#endif

#ifdef L_divsi3
	.text
	.globl ___divsi3
	.type  ___divsi3,@function
___divsi3:
	add -8,sp
	st.w r31,4[sp]
	st.w r22,0[sp]
	mov 1,r22
	tst r7,r7
	bp .L3
	subr r0,r7
	subr r0,r22
.L3:
	tst r6,r6
	bp .L4
	subr r0,r6
	subr r0,r22
.L4:
	jarl ___udivsi3,r31
	cmp r0,r22
	bp .L7
	subr r0,r10
.L7:
	ld.w 0[sp],r22
	ld.w 4[sp],r31
	add 8,sp
	jmp [r31]
	.size ___divsi3,.-___divsi3
#endif

#ifdef  L_umodsi3
	.text
	.globl ___umodsi3
	.type  ___umodsi3,@function
___umodsi3:
	add -12,sp
	st.w r31,8[sp]
	st.w r7,4[sp]
	st.w r6,0[sp]
	jarl ___udivsi3,r31
	ld.w 4[sp],r7
	mov r10,r6
	jarl ___mulsi3,r31
	ld.w 0[sp],r6
	subr r6,r10
	ld.w 8[sp],r31
	add 12,sp
	jmp [r31]
	.size ___umodsi3,.-___umodsi3
#endif /* L_umodsi3 */

#ifdef  L_modsi3
	.text
	.globl ___modsi3
	.type  ___modsi3,@function
___modsi3:
	add -12,sp
	st.w r31,8[sp]
	st.w r7,4[sp]
	st.w r6,0[sp]
	jarl ___divsi3,r31
	ld.w 4[sp],r7
	mov r10,r6
	jarl ___mulsi3,r31
	ld.w 0[sp],r6
	subr r6,r10
	ld.w 8[sp],r31
	add 12,sp
	jmp [r31]
	.size ___modsi3,.-___modsi3
#endif /* L_modsi3 */

#ifdef	L_save_2
	.text
	.align	2
	.globl	__save_r2_r29
	.type	__save_r2_r29,@function
	/* Allocate space and save registers 2, 20 .. 29 on the stack */
	/* Called via:	jalr __save_r2_r29,r10 */
__save_r2_r29:
	mov	ep,r1
	addi	-44,sp,sp
	mov	sp,ep
	sst.w	r29,0[ep]
	sst.w	r28,4[ep]
	sst.w	r27,8[ep]
	sst.w	r26,12[ep]
	sst.w	r25,16[ep]
	sst.w	r24,20[ep]
	sst.w	r23,24[ep]
	sst.w	r22,28[ep]
	sst.w	r21,32[ep]
	sst.w	r20,36[ep]
	sst.w	r2,40[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r2_r29,.-__save_r2_r29

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r2_r29 */
	.align	2
	.globl	__return_r2_r29
	.type	__return_r2_r29,@function
__return_r2_r29:
	mov	ep,r1
	mov	sp,ep
	sld.w	0[ep],r29
	sld.w	4[ep],r28
	sld.w	8[ep],r27
	sld.w	12[ep],r26
	sld.w	16[ep],r25
	sld.w	20[ep],r24
	sld.w	24[ep],r23
	sld.w	28[ep],r22
	sld.w	32[ep],r21
	sld.w	36[ep],r20
	sld.w	40[ep],r2
	addi	44,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r2_r29,.-__return_r2_r29
#endif /* L_save_2 */

#ifdef	L_save_20
	.text
	.align	2
	.globl	__save_r20_r29
	.type	__save_r20_r29,@function
	/* Allocate space and save registers 20 .. 29 on the stack */
	/* Called via:	jalr __save_r20_r29,r10 */
__save_r20_r29:
	mov	ep,r1
	addi	-40,sp,sp
	mov	sp,ep
	sst.w	r29,0[ep]
	sst.w	r28,4[ep]
	sst.w	r27,8[ep]
	sst.w	r26,12[ep]
	sst.w	r25,16[ep]
	sst.w	r24,20[ep]
	sst.w	r23,24[ep]
	sst.w	r22,28[ep]
	sst.w	r21,32[ep]
	sst.w	r20,36[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r20_r29,.-__save_r20_r29

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r20_r29 */
	.align	2
	.globl	__return_r20_r29
	.type	__return_r20_r29,@function
__return_r20_r29:
	mov	ep,r1
	mov	sp,ep
	sld.w	0[ep],r29
	sld.w	4[ep],r28
	sld.w	8[ep],r27
	sld.w	12[ep],r26
	sld.w	16[ep],r25
	sld.w	20[ep],r24
	sld.w	24[ep],r23
	sld.w	28[ep],r22
	sld.w	32[ep],r21
	sld.w	36[ep],r20
	addi	40,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r20_r29,.-__return_r20_r29
#endif /* L_save_20 */

#ifdef	L_save_21
	.text
	.align	2
	.globl	__save_r21_r29
	.type	__save_r21_r29,@function
	/* Allocate space and save registers 21 .. 29 on the stack */
	/* Called via:	jalr __save_r21_r29,r10 */
__save_r21_r29:
	mov	ep,r1
	addi	-36,sp,sp
	mov	sp,ep
	sst.w	r29,0[ep]
	sst.w	r28,4[ep]
	sst.w	r27,8[ep]
	sst.w	r26,12[ep]
	sst.w	r25,16[ep]
	sst.w	r24,20[ep]
	sst.w	r23,24[ep]
	sst.w	r22,28[ep]
	sst.w	r21,32[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r21_r29,.-__save_r21_r29

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r21_r29 */
	.align	2
	.globl	__return_r21_r29
	.type	__return_r21_r29,@function
__return_r21_r29:
	mov	ep,r1
	mov	sp,ep
	sld.w	0[ep],r29
	sld.w	4[ep],r28
	sld.w	8[ep],r27
	sld.w	12[ep],r26
	sld.w	16[ep],r25
	sld.w	20[ep],r24
	sld.w	24[ep],r23
	sld.w	28[ep],r22
	sld.w	32[ep],r21
	addi	36,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r21_r29,.-__return_r21_r29
#endif /* L_save_21 */

#ifdef	L_save_22
	.text
	.align	2
	.globl	__save_r22_r29
	.type	__save_r22_r29,@function
	/* Allocate space and save registers 22 .. 29 on the stack */
	/* Called via:	jalr __save_r22_r29,r10 */
__save_r22_r29:
	mov	ep,r1
	addi	-32,sp,sp
	mov	sp,ep
	sst.w	r29,0[ep]
	sst.w	r28,4[ep]
	sst.w	r27,8[ep]
	sst.w	r26,12[ep]
	sst.w	r25,16[ep]
	sst.w	r24,20[ep]
	sst.w	r23,24[ep]
	sst.w	r22,28[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r22_r29,.-__save_r22_r29

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r22_r29 */
	.align	2
	.globl	__return_r22_r29
	.type	__return_r22_r29,@function
__return_r22_r29:
	mov	ep,r1
	mov	sp,ep
	sld.w	0[ep],r29
	sld.w	4[ep],r28
	sld.w	8[ep],r27
	sld.w	12[ep],r26
	sld.w	16[ep],r25
	sld.w	20[ep],r24
	sld.w	24[ep],r23
	sld.w	28[ep],r22
	addi	32,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r22_r29,.-__return_r22_r29
#endif /* L_save_22 */

#ifdef	L_save_23
	.text
	.align	2
	.globl	__save_r23_r29
	.type	__save_r23_r29,@function
	/* Allocate space and save registers 23 .. 29 on the stack */
	/* Called via:	jalr __save_r23_r29,r10 */
__save_r23_r29:
	mov	ep,r1
	addi	-28,sp,sp
	mov	sp,ep
	sst.w	r29,0[ep]
	sst.w	r28,4[ep]
	sst.w	r27,8[ep]
	sst.w	r26,12[ep]
	sst.w	r25,16[ep]
	sst.w	r24,20[ep]
	sst.w	r23,24[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r23_r29,.-__save_r23_r29

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r23_r29 */
	.align	2
	.globl	__return_r23_r29
	.type	__return_r23_r29,@function
__return_r23_r29:
	mov	ep,r1
	mov	sp,ep
	sld.w	0[ep],r29
	sld.w	4[ep],r28
	sld.w	8[ep],r27
	sld.w	12[ep],r26
	sld.w	16[ep],r25
	sld.w	20[ep],r24
	sld.w	24[ep],r23
	addi	28,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r23_r29,.-__return_r23_r29
#endif /* L_save_23 */

#ifdef	L_save_24
	.text
	.align	2
	.globl	__save_r24_r29
	.type	__save_r24_r29,@function
	/* Allocate space and save registers 24 .. 29 on the stack */
	/* Called via:	jalr __save_r24_r29,r10 */
__save_r24_r29:
	mov	ep,r1
	addi	-24,sp,sp
	mov	sp,ep
	sst.w	r29,0[ep]
	sst.w	r28,4[ep]
	sst.w	r27,8[ep]
	sst.w	r26,12[ep]
	sst.w	r25,16[ep]
	sst.w	r24,20[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r24_r29,.-__save_r24_r29

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r24_r29 */
	.align	2
	.globl	__return_r24_r29
	.type	__return_r24_r29,@function
__return_r24_r29:
	mov	ep,r1
	mov	sp,ep
	sld.w	0[ep],r29
	sld.w	4[ep],r28
	sld.w	8[ep],r27
	sld.w	12[ep],r26
	sld.w	16[ep],r25
	sld.w	20[ep],r24
	addi	24,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r24_r29,.-__return_r24_r29
#endif /* L_save_24 */

#ifdef	L_save_25
	.text
	.align	2
	.globl	__save_r25_r29
	.type	__save_r25_r29,@function
	/* Allocate space and save registers 25 .. 29 on the stack */
	/* Called via:	jalr __save_r25_r29,r10 */
__save_r25_r29:
	mov	ep,r1
	addi	-20,sp,sp
	mov	sp,ep
	sst.w	r29,0[ep]
	sst.w	r28,4[ep]
	sst.w	r27,8[ep]
	sst.w	r26,12[ep]
	sst.w	r25,16[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r25_r29,.-__save_r25_r29

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r25_r29 */
	.align	2
	.globl	__return_r25_r29
	.type	__return_r25_r29,@function
__return_r25_r29:
	mov	ep,r1
	mov	sp,ep
	sld.w	0[ep],r29
	sld.w	4[ep],r28
	sld.w	8[ep],r27
	sld.w	12[ep],r26
	sld.w	16[ep],r25
	addi	20,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r25_r29,.-__return_r25_r29
#endif /* L_save_25 */

#ifdef	L_save_26
	.text
	.align	2
	.globl	__save_r26_r29
	.type	__save_r26_r29,@function
	/* Allocate space and save registers 26 .. 29 on the stack */
	/* Called via:	jalr __save_r26_r29,r10 */
__save_r26_r29:
	mov	ep,r1
	add	-16,sp
	mov	sp,ep
	sst.w	r29,0[ep]
	sst.w	r28,4[ep]
	sst.w	r27,8[ep]
	sst.w	r26,12[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r26_r29,.-__save_r26_r29

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r26_r29 */
	.align	2
	.globl	__return_r26_r29
	.type	__return_r26_r29,@function
__return_r26_r29:
	mov	ep,r1
	mov	sp,ep
	sld.w	0[ep],r29
	sld.w	4[ep],r28
	sld.w	8[ep],r27
	sld.w	12[ep],r26
	addi	16,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r26_r29,.-__return_r26_r29
#endif /* L_save_26 */

#ifdef	L_save_27
	.text
	.align	2
	.globl	__save_r27_r29
	.type	__save_r27_r29,@function
	/* Allocate space and save registers 27 .. 29 on the stack */
	/* Called via:	jalr __save_r27_r29,r10 */
__save_r27_r29:
	add	-12,sp
	st.w	r29,0[sp]
	st.w	r28,4[sp]
	st.w	r27,8[sp]
	jmp	[r10]
	.size	__save_r27_r29,.-__save_r27_r29

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r27_r29 */
	.align	2
	.globl	__return_r27_r29
	.type	__return_r27_r29,@function
__return_r27_r29:
	ld.w	0[sp],r29
	ld.w	4[sp],r28
	ld.w	8[sp],r27
	add	12,sp
	jmp	[r31]
	.size	__return_r27_r29,.-__return_r27_r29
#endif /* L_save_27 */

#ifdef	L_save_28
	.text
	.align	2
	.globl	__save_r28_r29
	.type	__save_r28_r29,@function
	/* Allocate space and save registers 28,29 on the stack */
	/* Called via:	jalr __save_r28_r29,r10 */
__save_r28_r29:
	add	-8,sp
	st.w	r29,0[sp]
	st.w	r28,4[sp]
	jmp	[r10]
	.size	__save_r28_r29,.-__save_r28_r29

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r28_r29 */
	.align	2
	.globl	__return_r28_r29
	.type	__return_r28_r29,@function
__return_r28_r29:
	ld.w	0[sp],r29
	ld.w	4[sp],r28
	add	8,sp
	jmp	[r31]
	.size	__return_r28_r29,.-__return_r28_r29
#endif /* L_save_28 */

#ifdef	L_save_29
	.text
	.align	2
	.globl	__save_r29
	.type	__save_r29,@function
	/* Allocate space and save register 29 on the stack */
	/* Called via:	jalr __save_r29,r10 */
__save_r29:
	add	-4,sp
	st.w	r29,0[sp]
	jmp	[r10]
	.size	__save_r29,.-__save_r29

	/* Restore saved register 29, deallocate stack and return to the user */
	/* Called via:	jr __return_r29 */
	.align	2
	.globl	__return_r29
	.type	__return_r29,@function
__return_r29:
	ld.w	0[sp],r29
	add	4,sp
	jmp	[r31]
	.size	__return_r29,.-__return_r29
#endif /* L_save_28 */

#ifdef	L_save_2c
	.text
	.align	2
	.globl	__save_r2_r31
	.type	__save_r2_r31,@function
	/* Allocate space and save registers 20 .. 29, 31 on the stack */
	/* Also allocate space for the argument save area */
	/* Called via:	jalr __save_r2_r31,r10 */
__save_r2_r31:
	mov	ep,r1
	addi	-64,sp,sp
	mov	sp,ep
	sst.w	r29,16[ep]
	sst.w	r28,20[ep]
	sst.w	r27,24[ep]
	sst.w	r26,28[ep]
	sst.w	r25,32[ep]
	sst.w	r24,36[ep]
	sst.w	r23,40[ep]
	sst.w	r22,44[ep]
	sst.w	r21,48[ep]
	sst.w	r20,52[ep]
	sst.w	r2,56[ep]
	sst.w	r31,60[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r2_r31,.-__save_r2_r31

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r20_r31 */
	.align	2
	.globl	__return_r2_r31
	.type	__return_r2_r31,@function
__return_r2_r31:
	mov	ep,r1
	mov	sp,ep
	sld.w	16[ep],r29
	sld.w	20[ep],r28
	sld.w	24[ep],r27
	sld.w	28[ep],r26
	sld.w	32[ep],r25
	sld.w	36[ep],r24
	sld.w	40[ep],r23
	sld.w	44[ep],r22
	sld.w	48[ep],r21
	sld.w	52[ep],r20
	sld.w	56[ep],r2
	sld.w	60[ep],r31
	addi	64,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r2_r31,.-__return_r2_r31
#endif /* L_save_2c */

#ifdef	L_save_20c
	.text
	.align	2
	.globl	__save_r20_r31
	.type	__save_r20_r31,@function
	/* Allocate space and save registers 20 .. 29, 31 on the stack */
	/* Also allocate space for the argument save area */
	/* Called via:	jalr __save_r20_r31,r10 */
__save_r20_r31:
	mov	ep,r1
	addi	-60,sp,sp
	mov	sp,ep
	sst.w	r29,16[ep]
	sst.w	r28,20[ep]
	sst.w	r27,24[ep]
	sst.w	r26,28[ep]
	sst.w	r25,32[ep]
	sst.w	r24,36[ep]
	sst.w	r23,40[ep]
	sst.w	r22,44[ep]
	sst.w	r21,48[ep]
	sst.w	r20,52[ep]
	sst.w	r31,56[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r20_r31,.-__save_r20_r31

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r20_r31 */
	.align	2
	.globl	__return_r20_r31
	.type	__return_r20_r31,@function
__return_r20_r31:
	mov	ep,r1
	mov	sp,ep
	sld.w	16[ep],r29
	sld.w	20[ep],r28
	sld.w	24[ep],r27
	sld.w	28[ep],r26
	sld.w	32[ep],r25
	sld.w	36[ep],r24
	sld.w	40[ep],r23
	sld.w	44[ep],r22
	sld.w	48[ep],r21
	sld.w	52[ep],r20
	sld.w	56[ep],r31
	addi	60,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r20_r31,.-__return_r20_r31
#endif /* L_save_20c */

#ifdef	L_save_21c
	.text
	.align	2
	.globl	__save_r21_r31
	.type	__save_r21_r31,@function
	/* Allocate space and save registers 21 .. 29, 31 on the stack */
	/* Also allocate space for the argument save area */
	/* Called via:	jalr __save_r21_r31,r10 */
__save_r21_r31:
	mov	ep,r1
	addi	-56,sp,sp
	mov	sp,ep
	sst.w	r29,16[ep]
	sst.w	r28,20[ep]
	sst.w	r27,24[ep]
	sst.w	r26,28[ep]
	sst.w	r25,32[ep]
	sst.w	r24,36[ep]
	sst.w	r23,40[ep]
	sst.w	r22,44[ep]
	sst.w	r21,48[ep]
	sst.w	r31,52[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r21_r31,.-__save_r21_r31

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r21_r31 */
	.align	2
	.globl	__return_r21_r31
	.type	__return_r21_r31,@function
__return_r21_r31:
	mov	ep,r1
	mov	sp,ep
	sld.w	16[ep],r29
	sld.w	20[ep],r28
	sld.w	24[ep],r27
	sld.w	28[ep],r26
	sld.w	32[ep],r25
	sld.w	36[ep],r24
	sld.w	40[ep],r23
	sld.w	44[ep],r22
	sld.w	48[ep],r21
	sld.w	52[ep],r31
	addi	56,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r21_r31,.-__return_r21_r31
#endif /* L_save_21c */

#ifdef	L_save_22c
	.text
	.align	2
	.globl	__save_r22_r31
	.type	__save_r22_r31,@function
	/* Allocate space and save registers 22 .. 29, 31 on the stack */
	/* Also allocate space for the argument save area */
	/* Called via:	jalr __save_r22_r31,r10 */
__save_r22_r31:
	mov	ep,r1
	addi	-52,sp,sp
	mov	sp,ep
	sst.w	r29,16[ep]
	sst.w	r28,20[ep]
	sst.w	r27,24[ep]
	sst.w	r26,28[ep]
	sst.w	r25,32[ep]
	sst.w	r24,36[ep]
	sst.w	r23,40[ep]
	sst.w	r22,44[ep]
	sst.w	r31,48[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r22_r31,.-__save_r22_r31

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r22_r31 */
	.align	2
	.globl	__return_r22_r31
	.type	__return_r22_r31,@function
__return_r22_r31:
	mov	ep,r1
	mov	sp,ep
	sld.w	16[ep],r29
	sld.w	20[ep],r28
	sld.w	24[ep],r27
	sld.w	28[ep],r26
	sld.w	32[ep],r25
	sld.w	36[ep],r24
	sld.w	40[ep],r23
	sld.w	44[ep],r22
	sld.w	48[ep],r31
	addi	52,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r22_r31,.-__return_r22_r31
#endif /* L_save_22c */

#ifdef	L_save_23c
	.text
	.align	2
	.globl	__save_r23_r31
	.type	__save_r23_r31,@function
	/* Allocate space and save registers 23 .. 29, 31 on the stack */
	/* Also allocate space for the argument save area */
	/* Called via:	jalr __save_r23_r31,r10 */
__save_r23_r31:
	mov	ep,r1
	addi	-48,sp,sp
	mov	sp,ep
	sst.w	r29,16[ep]
	sst.w	r28,20[ep]
	sst.w	r27,24[ep]
	sst.w	r26,28[ep]
	sst.w	r25,32[ep]
	sst.w	r24,36[ep]
	sst.w	r23,40[ep]
	sst.w	r31,44[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r23_r31,.-__save_r23_r31

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r23_r31 */
	.align	2
	.globl	__return_r23_r31
	.type	__return_r23_r31,@function
__return_r23_r31:
	mov	ep,r1
	mov	sp,ep
	sld.w	16[ep],r29
	sld.w	20[ep],r28
	sld.w	24[ep],r27
	sld.w	28[ep],r26
	sld.w	32[ep],r25
	sld.w	36[ep],r24
	sld.w	40[ep],r23
	sld.w	44[ep],r31
	addi	48,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r23_r31,.-__return_r23_r31
#endif /* L_save_23c */

#ifdef	L_save_24c
	.text
	.align	2
	.globl	__save_r24_r31
	.type	__save_r24_r31,@function
	/* Allocate space and save registers 24 .. 29, 31 on the stack */
	/* Also allocate space for the argument save area */
	/* Called via:	jalr __save_r24_r31,r10 */
__save_r24_r31:
	mov	ep,r1
	addi	-44,sp,sp
	mov	sp,ep
	sst.w	r29,16[ep]
	sst.w	r28,20[ep]
	sst.w	r27,24[ep]
	sst.w	r26,28[ep]
	sst.w	r25,32[ep]
	sst.w	r24,36[ep]
	sst.w	r31,40[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r24_r31,.-__save_r24_r31

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r24_r31 */
	.align	2
	.globl	__return_r24_r31
	.type	__return_r24_r31,@function
__return_r24_r31:
	mov	ep,r1
	mov	sp,ep
	sld.w	16[ep],r29
	sld.w	20[ep],r28
	sld.w	24[ep],r27
	sld.w	28[ep],r26
	sld.w	32[ep],r25
	sld.w	36[ep],r24
	sld.w	40[ep],r31
	addi	44,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r24_r31,.-__return_r24_r31
#endif /* L_save_24c */

#ifdef	L_save_25c
	.text
	.align	2
	.globl	__save_r25_r31
	.type	__save_r25_r31,@function
	/* Allocate space and save registers 25 .. 29, 31 on the stack */
	/* Also allocate space for the argument save area */
	/* Called via:	jalr __save_r25_r31,r10 */
__save_r25_r31:
	mov	ep,r1
	addi	-40,sp,sp
	mov	sp,ep
	sst.w	r29,16[ep]
	sst.w	r28,20[ep]
	sst.w	r27,24[ep]
	sst.w	r26,28[ep]
	sst.w	r25,32[ep]
	sst.w	r31,36[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r25_r31,.-__save_r25_r31

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r25_r31 */
	.align	2
	.globl	__return_r25_r31
	.type	__return_r25_r31,@function
__return_r25_r31:
	mov	ep,r1
	mov	sp,ep
	sld.w	16[ep],r29
	sld.w	20[ep],r28
	sld.w	24[ep],r27
	sld.w	28[ep],r26
	sld.w	32[ep],r25
	sld.w	36[ep],r31
	addi	40,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r25_r31,.-__return_r25_r31
#endif /* L_save_25c */

#ifdef	L_save_26c
	.text
	.align	2
	.globl	__save_r26_r31
	.type	__save_r26_r31,@function
	/* Allocate space and save registers 26 .. 29, 31 on the stack */
	/* Also allocate space for the argument save area */
	/* Called via:	jalr __save_r26_r31,r10 */
__save_r26_r31:
	mov	ep,r1
	addi	-36,sp,sp
	mov	sp,ep
	sst.w	r29,16[ep]
	sst.w	r28,20[ep]
	sst.w	r27,24[ep]
	sst.w	r26,28[ep]
	sst.w	r31,32[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r26_r31,.-__save_r26_r31

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r26_r31 */
	.align	2
	.globl	__return_r26_r31
	.type	__return_r26_r31,@function
__return_r26_r31:
	mov	ep,r1
	mov	sp,ep
	sld.w	16[ep],r29
	sld.w	20[ep],r28
	sld.w	24[ep],r27
	sld.w	28[ep],r26
	sld.w	32[ep],r31
	addi	36,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r26_r31,.-__return_r26_r31
#endif /* L_save_26c */

#ifdef	L_save_27c
	.text
	.align	2
	.globl	__save_r27_r31
	.type	__save_r27_r31,@function
	/* Allocate space and save registers 27 .. 29, 31 on the stack */
	/* Also allocate space for the argument save area */
	/* Called via:	jalr __save_r27_r31,r10 */
__save_r27_r31:
	mov	ep,r1
	addi	-32,sp,sp
	mov	sp,ep
	sst.w	r29,16[ep]
	sst.w	r28,20[ep]
	sst.w	r27,24[ep]
	sst.w	r31,28[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r27_r31,.-__save_r27_r31

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r27_r31 */
	.align	2
	.globl	__return_r27_r31
	.type	__return_r27_r31,@function
__return_r27_r31:
	mov	ep,r1
	mov	sp,ep
	sld.w	16[ep],r29
	sld.w	20[ep],r28
	sld.w	24[ep],r27
	sld.w	28[ep],r31
	addi	32,sp,sp
	mov	r1,ep
	jmp	[r31]
	.size	__return_r27_r31,.-__return_r27_r31
#endif /* L_save_27c */

#ifdef	L_save_28c
	.text
	.align	2
	.globl	__save_r28_r31
	.type	__save_r28_r31,@function
	/* Allocate space and save registers 28 .. 29, 31 on the stack */
	/* Also allocate space for the argument save area */
	/* Called via:	jalr __save_r28_r31,r10 */
__save_r28_r31:
	addi	-28,sp,sp
	st.w	r29,16[sp]
	st.w	r28,20[sp]
	st.w	r31,24[sp]
	jmp	[r10]
	.size	__save_r28_r31,.-__save_r28_r31

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r28_r31 */
	.align	2
	.globl	__return_r28_r31
	.type	__return_r28_r31,@function
__return_r28_r31:
	ld.w	16[sp],r29
	ld.w	20[sp],r28
	ld.w	24[sp],r31
	addi	28,sp,sp
	jmp	[r31]
	.size	__return_r28_r31,.-__return_r28_r31
#endif /* L_save_28c */

#ifdef	L_save_29c
	.text
	.align	2
	.globl	__save_r29_r31
	.type	__save_r29_r31,@function
	/* Allocate space and save registers 29 & 31 on the stack */
	/* Also allocate space for the argument save area */
	/* Called via:	jalr __save_r29_r31,r10 */
__save_r29_r31:
	addi	-24,sp,sp
	st.w	r29,16[sp]
	st.w	r31,20[sp]
	jmp	[r10]
	.size	__save_r29_r31,.-__save_r29_r31

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r29_r31 */
	.align	2
	.globl	__return_r29_r31
	.type	__return_r29_r31,@function
__return_r29_r31:
	ld.w	16[sp],r29
	ld.w	20[sp],r31
	addi	24,sp,sp
	jmp	[r31]
	.size	__return_r29_r31,.-__return_r29_r31
#endif /* L_save_29c */

#ifdef	L_save_31c
	.text
	.align	2
	.globl	__save_r31
	.type	__save_r31,@function
	/* Allocate space and save register 31 on the stack */
	/* Also allocate space for the argument save area */
	/* Called via:	jalr __save_r29_r31,r10 */
__save_r31:
	addi	-20,sp,sp
	st.w	r31,16[sp]
	jmp	[r10]
	.size	__save_r31,.-__save_r31

	/* Restore saved registers, deallocate stack and return to the user */
	/* Called via:	jr __return_r31 */
	.align	2
	.globl	__return_r31
	.type	__return_r31,@function
__return_r31:
	ld.w	16[sp],r31
	addi	20,sp,sp
	jmp	[r31]
	.size	__return_r29_r31,.-__return_r29_r31
#endif /* L_save_31c */

#ifdef L_save_varargs
	.text
	.align	2
	.globl	__save_r6_r9
	.type	__save_r6_r9,@function
	/* Save registers 6 .. 9 on the stack for variable argument functions */
	/* Called via:	jalr __save_r6_r9,r10 */
__save_r6_r9:
	mov	ep,r1
	mov	sp,ep
	sst.w	r6,0[ep]
	sst.w	r7,4[ep]
	sst.w	r8,8[ep]
	sst.w	r9,12[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_r6_r9,.-__save_r6_r9
#endif /* L_save_varargs */

#ifdef	L_save_interrupt
	.text
	.align	2
	.globl	__save_interrupt
	.type	__save_interrupt,@function
	/* Save registers r1, r4 on stack and load up with expected values */
	/* Note, 12 bytes of stack have already been allocated. */
	/* Called via:	jalr __save_interrupt,r10 */
__save_interrupt:
	st.w	ep,0[sp]
	st.w	gp,4[sp]
	st.w	r1,8[sp]
	movhi	hi(__ep),r0,ep
	movea	lo(__ep),ep,ep
	movhi	hi(__gp),r0,gp
	movea	lo(__gp),gp,gp
	jmp	[r10]
	.size	__save_interrupt,.-__save_interrupt

	/* Restore saved registers, deallocate stack and return from the interrupt */
	/* Called via:	jr __return_interrupt */
	.align	2
	.globl	__return_interrupt
	.type	__return_interrupt,@function
__return_interrupt:
	ld.w	0[sp],ep
	ld.w	4[sp],gp
	ld.w	8[sp],r1
	ld.w	12[sp],r10
	addi	16,sp,sp
	reti
	.size	__return_interrupt,.-__return_interrupt
#endif /* L_save_interrupt */

#ifdef L_save_all_interrupt
	.text
	.align	2
	.globl	__save_all_interrupt
	.type	__save_all_interrupt,@function
	/* Save all registers except for those saved in __save_interrupt */
	/* allocate enough stack for all of the registers & 16 bytes of space */
	/* Called via:	jalr __save_all_interrupt,r10 */
__save_all_interrupt:
	addi	-120,sp,sp
	mov	ep,r1
	mov	sp,ep
	sst.w	r31,116[ep]
	sst.w	r2,112[ep]
	sst.w	gp,108[ep]
	sst.w	r6,104[ep]
	sst.w	r7,100[ep]
	sst.w	r8,96[ep]
	sst.w	r9,92[ep]
	sst.w	r11,88[ep]
	sst.w	r12,84[ep]
	sst.w	r13,80[ep]
	sst.w	r14,76[ep]
	sst.w	r15,72[ep]
	sst.w	r16,68[ep]
	sst.w	r17,64[ep]
	sst.w	r18,60[ep]
	sst.w	r19,56[ep]
	sst.w	r20,52[ep]
	sst.w	r21,48[ep]
	sst.w	r22,44[ep]
	sst.w	r23,40[ep]
	sst.w	r24,36[ep]
	sst.w	r25,32[ep]
	sst.w	r26,28[ep]
	sst.w	r27,24[ep]
	sst.w	r28,20[ep]
	sst.w	r29,16[ep]
	mov	r1,ep
	jmp	[r10]
	.size	__save_all_interrupt,.-__save_all_interrupt

	.globl	__restore_all_interrupt
	.type	__restore_all_interrupt,@function
	/* Restore all registers saved in __save_all_interrupt */
	/* & deallocate the stack space */
	/* Called via:	jalr __restore_all_interrupt,r10 */
__restore_all_interrupt:
	mov	ep,r1
	mov	sp,ep
	sld.w	116[ep],r31
	sld.w	112[ep],r2
	sld.w	108[ep],gp
	sld.w	104[ep],r6
	sld.w	100[ep],r7
	sld.w	96[ep],r8
	sld.w	92[ep],r9
	sld.w	88[ep],r11
	sld.w	84[ep],r12
	sld.w	80[ep],r13
	sld.w	76[ep],r14
	sld.w	72[ep],r15
	sld.w	68[ep],r16
	sld.w	64[ep],r17
	sld.w	60[ep],r18
	sld.w	56[ep],r19
	sld.w	52[ep],r20
	sld.w	48[ep],r21
	sld.w	44[ep],r22
	sld.w	40[ep],r23
	sld.w	36[ep],r24
	sld.w	32[ep],r25
	sld.w	28[ep],r26
	sld.w	24[ep],r27
	sld.w	20[ep],r28
	sld.w	16[ep],r29
	mov	r1,ep
	addi	120,sp,sp
	jmp	[r10]
	.size	__restore_all_interrupt,.-__restore_all_interrupt
#endif /* L_save_all_interrupt */
