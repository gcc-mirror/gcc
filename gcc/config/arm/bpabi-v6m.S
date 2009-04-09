/* Miscellaneous BPABI functions.  ARMv6M implementation

   Copyright (C) 2006, 2008, 2009  Free Software Foundation, Inc.
   Contributed by CodeSourcery.

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

FUNC_START aeabi_lcmp
	cmp	xxh, yyh
	beq	1f
	bgt	2f
	mov	r0, #1
	neg	r0, r0
	RET
2:
	mov	r0, #1
	RET
1:
	sub	r0, xxl, yyl
	beq	1f
	bhi	2f
	mov	r0, #1
	neg	r0, r0
	RET
2:
	mov	r0, #1
1:
	RET
	FUNC_END aeabi_lcmp

#endif /* L_aeabi_lcmp */
	
#ifdef L_aeabi_ulcmp

FUNC_START aeabi_ulcmp
	cmp	xxh, yyh
	bne	1f
	sub	r0, xxl, yyl
	beq	2f
1:
	bcs	1f
	mov	r0, #1
	neg	r0, r0
	RET
1:
	mov	r0, #1
2:
	RET
	FUNC_END aeabi_ulcmp

#endif /* L_aeabi_ulcmp */

#ifdef L_aeabi_ldivmod

FUNC_START aeabi_ldivmod
	push {r0, r1}
	mov r0, sp
	push {r0, lr}
	ldr r0, [sp, #8]
	bl SYM(__gnu_ldivmod_helper)
	ldr r3, [sp, #4]
	mov lr, r3
	add sp, sp, #8
	pop {r2, r3}
	RET
	FUNC_END aeabi_ldivmod

#endif /* L_aeabi_ldivmod */

#ifdef L_aeabi_uldivmod

FUNC_START aeabi_uldivmod
	push {r0, r1}
	mov r0, sp
	push {r0, lr}
	ldr r0, [sp, #8]
	bl SYM(__gnu_uldivmod_helper)
	ldr r3, [sp, #4]
	mov lr, r3
	add sp, sp, #8
	pop {r2, r3}
	RET
	FUNC_END aeabi_uldivmod
	
#endif /* L_aeabi_uldivmod */

#ifdef L_arm_addsubsf3

FUNC_START aeabi_frsub

      push	{r4, lr}
      mov	r4, #1
      lsl	r4, #31
      eor	r0, r0, r4
      bl	__aeabi_fadd
      pop	{r4, pc}

      FUNC_END aeabi_frsub

#endif /* L_arm_addsubsf3 */

#ifdef L_arm_cmpsf2

FUNC_START aeabi_cfrcmple

	mov	ip, r0
	mov	r0, r1
	mov	r1, ip
	b	6f

FUNC_START aeabi_cfcmpeq
FUNC_ALIAS aeabi_cfcmple aeabi_cfcmpeq

	@ The status-returning routines are required to preserve all
	@ registers except ip, lr, and cpsr.
6:	push	{r0, r1, r2, r3, r4, lr}
	bl	__lesf2
	@ Set the Z flag correctly, and the C flag unconditionally.
	cmp	r0, #0
	@ Clear the C flag if the return value was -1, indicating
	@ that the first operand was smaller than the second.
	bmi 1f
	mov	r1, #0
	cmn	r0, r1
1:
	pop	{r0, r1, r2, r3, r4, pc}

	FUNC_END aeabi_cfcmple
	FUNC_END aeabi_cfcmpeq
	FUNC_END aeabi_cfrcmple

FUNC_START	aeabi_fcmpeq

	push	{r4, lr}
	bl	__eqsf2
	neg	r0, r0
	add	r0, r0, #1
	pop	{r4, pc}

	FUNC_END aeabi_fcmpeq

.macro COMPARISON cond, helper, mode=sf2
FUNC_START	aeabi_fcmp\cond

	push	{r4, lr}
	bl	__\helper\mode
	cmp	r0, #0
	b\cond	1f
	mov	r0, #0
	pop	{r4, pc}
1:
	mov	r0, #1
	pop	{r4, pc}

	FUNC_END aeabi_fcmp\cond
.endm

COMPARISON lt, le
COMPARISON le, le
COMPARISON gt, ge
COMPARISON ge, ge

#endif /* L_arm_cmpsf2 */

#ifdef L_arm_addsubdf3

FUNC_START aeabi_drsub

      push	{r4, lr}
      mov	r4, #1
      lsl	r4, #31
      eor	xxh, xxh, r4
      bl	__aeabi_dadd
      pop	{r4, pc}

      FUNC_END aeabi_drsub

#endif /* L_arm_addsubdf3 */

#ifdef L_arm_cmpdf2

FUNC_START aeabi_cdrcmple

	mov	ip, r0
	mov	r0, r2
	mov	r2, ip
	mov	ip, r1
	mov	r1, r3
	mov	r3, ip
	b	6f

FUNC_START aeabi_cdcmpeq
FUNC_ALIAS aeabi_cdcmple aeabi_cdcmpeq

	@ The status-returning routines are required to preserve all
	@ registers except ip, lr, and cpsr.
6:	push	{r0, r1, r2, r3, r4, lr}
	bl	__ledf2
	@ Set the Z flag correctly, and the C flag unconditionally.
	cmp	r0, #0
	@ Clear the C flag if the return value was -1, indicating
	@ that the first operand was smaller than the second.
	bmi 1f
	mov	r1, #0
	cmn	r0, r1
1:
	pop	{r0, r1, r2, r3, r4, pc}

	FUNC_END aeabi_cdcmple
	FUNC_END aeabi_cdcmpeq
	FUNC_END aeabi_cdrcmple

FUNC_START	aeabi_dcmpeq

	push	{r4, lr}
	bl	__eqdf2
	neg	r0, r0
	add	r0, r0, #1
	pop	{r4, pc}

	FUNC_END aeabi_dcmpeq

.macro COMPARISON cond, helper, mode=df2
FUNC_START	aeabi_dcmp\cond

	push	{r4, lr}
	bl	__\helper\mode
	cmp	r0, #0
	b\cond	1f
	mov	r0, #0
	pop	{r4, pc}
1:
	mov	r0, #1
	pop	{r4, pc}

	FUNC_END aeabi_dcmp\cond
.endm

COMPARISON lt, le
COMPARISON le, le
COMPARISON gt, ge
COMPARISON ge, ge

#endif /* L_arm_cmpdf2 */
