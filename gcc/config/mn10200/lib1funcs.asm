/* libgcc1 routines for Matsushita mn10200.
   Copyright (C) 1997 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#ifdef L_divhi3
	/* Derive signed division/modulo from unsigned "divu" instruction.  */
	.text
	.globl ___divhi3
	.type ___divhi3,@function
___divhi3:

	/* We're going to need some scratch registers, so save d2/d3
	   into the stack.  */
	add -8,a3
	movx d2,(0,a3)
	movx d3,(4,a3)

	/* Loading zeros into registers now allows us to use them
	   in the compare instructions, which saves a total of
	   two bytes (egad).  */
	sub d3,d3
	sub d2,d2
	sub a0,a0

	/* If first operand is negative, then make it positive.
	   It will be contained in d2 just before .L1. 

	   a0 tells us if the first operand was negated.  */
	cmp d2,d0
	bge .L0
	sub d0,d2
	mov 1,a0
	bra .L1
.L0:
	mov d0,d2
.L1:
	/* If the second operand is negative, then make it positive.
	   It will be contained in d3 just before .L3. 

	   d0 tells us if the second operand was negated.  */
	cmp d3,d1
	bge .L2
	sub d1,d3
	mov 1,d0
	bra .L3
.L2:
	sub d0,d0
	mov d1,d3
.L3:
	/* Loading d1 with zero here allows us to save one byte
	   in the comparison below.  */

	sub d1,d1

	/* Make sure to clear the mdr register, then do the unsigned
	   division.  Result will be in d2/mdr.  */
	mov d1,mdr
	divu d3,d2

	/* Negate the remainder based on the first argument negation
	   flag only.  */
	cmp d1,a0
	beq .L4
	mov mdr,d3
	sub d3,d1
	bra .L5
.L4:
	mov mdr,d1

.L5:
	/* Negate the result if either, but not both of the inputs
	   were negated.  */
	mov a0,d3
	xor d3,d0
	beq .L6
	sub d0,d0
	sub d2,d0
	bra .L7
.L6:
	mov d2,d0
.L7:
	
	/* Restore our scratch registers, deallocate our stack and return.  */
	movx (0,a3),d2
	movx (4,a3),d3
	add 8,a3
	rts
	.size ___divhi3,.-___divhi3
#endif

#ifdef L_modhi3
	.text
	.globl ___modhi3
	.type ___modhi3,@function
___modhi3:
	jsr ___divhi3
	mov d1,d0
	rts
	.size ___modhi3,.-___modhi3
#endif

#ifdef L_addsi3
	.text
	.globl ___addsi3
	.type  ___addsi3,@function
___addsi3:
	add -4,a3
	movx d2,(0,a3)
	mov (8,a3),d2
	add d2,d0
	mov (10,a3),d2
	addc d2,d1
	movx (0,a3),d2
	add 4,a3
	rts

	.size ___addsi3,.-___addsi3
#endif

#ifdef L_subsi3
	.text
	.globl ___subsi3
	.type  ___subsi3,@function
___subsi3:
	add -4,a3
	movx d2,(0,a3)
	mov (8,a3),d2
	sub d2,d0
	mov (10,a3),d2
	subc d2,d1
	movx (0,a3),d2
	add 4,a3
	rts

	.size ___subsi3,.-___subsi3
#endif

#ifdef L_mulsi3
	.text
	.globl ___mulsi3
	.type  ___mulsi3,@function
___mulsi3:
	add -4,a3
	mov a1,(0,a3)
	mov d0,a0
	/* Multiply arg0 msb with arg1 lsb.
	   arg0 msb is in register d1,
	   arg1 lsb is in memory.  */
	mov (8,a3),d0
	mulu d0,d1
	mov d1,a1

	/* Multiply arg0 lsb with arg1 msb.
	   arg0 msb is in register a0,
	   arg1 lsb is in memory.  */
	mov a0,d0
	mov (10,a3),d1
	mulu d0,d1

	/* Add the cross products.  */
	add d1,a1

	/* Now multiply arg0 lsb with arg1 lsb.  */
	mov (8,a3),d1
	mulu d1,d0

	/* Add in the upper 16 bits to the cross product sum.  */
	mov mdr,d1
	add a1,d1
	mov (0,a3),a1
	add 4,a3
	rts

	.size ___mulsi3,.-___mulsi3
#endif

#ifdef  L_ashlsi3
	.text
	.globl ___ashlsi3
	.type  ___ashlsi3,@function
___ashlsi3:
	mov (4,a3),a0
	cmp 0,a0
	beq .L0
.L1:
	add d0,d0
	addc d1,d1
	add -1,a0
	bne .L1
.L0:
	rts

	.size ___ashlsi3,.-___ashlsi3
#endif

#ifdef  L_lshrsi3
	.text
	.globl ___lshrsi3
	.type  ___lshrsi3,@function
___lshrsi3:
	mov (4,a3),a0
	cmp 0,a0
	beq .L0
.L1:
	lsr d1
	ror d0
	add -1,a0
	bne .L1
.L0:
	rts

	.size ___lshrsi3,.-___lshrsi3
#endif

#ifdef  L_ashrsi3
	.text
	.globl ___ashrsi3
	.type  ___ashrsi3,@function
___ashrsi3:
	mov (4,a3),a0
	cmp 0,a0
	beq .L0
.L1:
	asr d1
	ror d0
	add -1,a0
	bne .L1
.L0:
	rts

	.size ___ashrsi3,.-___ashrsi3
#endif

/* All functions beyond this point pass their arguments in registers! */
#ifdef  L_negsi2_d0
	.text
	.globl ___negsi2_d0
	.type  ___negsi2_d0,@function
___negsi2_d0:
	add -8,a3
	movx d3,(0,a3)
	movx d2,(4,a3)
	mov d0,d2
	mov d1,d3
	sub d0,d0
	sub d1,d1
	sub d2,d0
	subc d3,d1
	movx (0,a3),d3
	movx (4,a3),d2
	add 8,a3
	rts

	.size ___negsi2_d0,.-___negsi2_d0
#endif

#ifdef  L_negsi2_d2
	.text
	.globl ___negsi2_d2
	.type  ___negsi2_d2,@function
___negsi2_d2:
	add -8,a3
	movx d1,(0,a3)
	movx d0,(4,a3)
	mov d2,d0
	mov d3,d1
	sub d2,d2
	sub d3,d3
	sub d0,d2
	subc d1,d3
	movx (0,a3),d1
	movx (4,a3),d0
	add 8,a3
	rts

	.size ___negsi2_d2,.-___negsi2_d2
#endif

#ifdef  L_zero_extendpsisi2_d0
	.text
	.globl ___zero_extendpsisi2_d0
	.type  ___zero_extendpsisi2_d0,@function
___zero_extendpsisi2_d0:
	add -4,a3
	movx d0,(0,a3)
	movbu (2,a3),d1
	add 4,a3
	rts

	.size ___zero_extendpsisi2_d0,.-___zero_extendpsisi2_d0
#endif

#ifdef  L_zero_extendpsisi2_d2
	.text
	.globl ___zero_extendpsisi2_d2
	.type  ___zero_extendpsisi2_d2,@function
___zero_extendpsisi2_d2:
	add -4,a3
	movx d2,(0,a3)
	movbu (2,a3),d3
	add 4,a3
	rts

	.size ___zero_extendpsisi2_d2,.-___zero_extendpsisi2_d2
#endif

#ifdef  L_sign_extendpsisi2_d0
	.text
	.globl ___sign_extendpsisi2_d0
	.type  ___sign_extendpsisi2_d0,@function
___sign_extendpsisi2_d0:
	add -4,a3
	movx d0,(0,a3)
	movb (2,a3),d1
	add 4,a3
	rts

	.size ___sign_extendpsisi2_d0,.-___sign_extendpsisi2_d0
#endif

#ifdef  L_sign_extendpsisi2_d2
	.text
	.globl ___sign_extendpsisi2_d2
	.type  ___sign_extendpsisi2_d2,@function
___sign_extendpsisi2_d2:
	add -4,a3
	movx d2,(0,a3)
	movb (2,a3),d3
	add 4,a3
	rts

	.size ___sign_extendpsisi2_d2,.-___sign_extendpsisi2_d2
#endif

#ifdef  L_truncsipsi2_d0_d0
	.text
	.globl ___truncsipsi2_d0_d0
	.type  ___truncsipsi2_d0_d0,@function
___truncsipsi2_d0_d0:
	add -4,a3
	mov d0,(a3)
	mov d1,(2,a3)
	movx (0,a3),d0
	add 4,a3
	rts

	.size ___truncsipsi2_d0_d0,.-___truncsipsi2_d0_d0
#endif

#ifdef  L_truncsipsi2_d0_d1
	.text
	.globl ___truncsipsi2_d0_d1
	.type  ___truncsipsi2_d0_d1,@function
___truncsipsi2_d0_d1:
	add -4,a3
	mov d0,(a3)
	mov d1,(2,a3)
	movx (0,a3),d1
	add 4,a3
	rts

	.size ___truncsipsi2_d0_d1,.-___truncsipsi2_d0_d1
#endif

#ifdef  L_truncsipsi2_d0_d2
	.text
	.globl ___truncsipsi2_d0_d2
	.type  ___truncsipsi2_d0_d2,@function
___truncsipsi2_d0_d2:
	add -4,a3
	mov d0,(a3)
	mov d1,(2,a3)
	movx (0,a3),d2
	add 4,a3
	rts

	.size ___truncsipsi2_d0_d2,.-___truncsipsi2_d0_d2
#endif

#ifdef  L_truncsipsi2_d0_d3
	.text
	.globl ___truncsipsi2_d0_d3
	.type  ___truncsipsi2_d0_d3,@function
___truncsipsi2_d0_d3:
	add -4,a3
	mov d0,(a3)
	mov d1,(2,a3)
	movx (0,a3),d3
	add 4,a3
	rts

	.size ___truncsipsi2_d0_d3,.-___truncsipsi2_d0_d3
#endif

#ifdef  L_truncsipsi2_d2_d0
	.text
	.globl ___truncsipsi2_d2_d0
	.type  ___truncsipsi2_d2_d0,@function
___truncsipsi2_d2_d0:
	add -4,a3
	mov d2,(a3)
	mov d3,(2,a3)
	movx (0,a3),d0
	add 4,a3
	rts

	.size ___truncsipsi2_d2_d0,.-___truncsipsi2_d2_d0
#endif

#ifdef  L_truncsipsi2_d2_d1
	.text
	.globl ___truncsipsi2_d2_d1
	.type  ___truncsipsi2_d2_d1,@function
___truncsipsi2_d2_d1:
	add -4,a3
	mov d2,(a3)
	mov d3,(2,a3)
	movx (0,a3),d1
	add 4,a3
	rts

	.size ___truncsipsi2_d2_d1,.-___truncsipsi2_d2_d1
#endif

#ifdef  L_truncsipsi2_d2_d2
	.text
	.globl ___truncsipsi2_d2_d2
	.type  ___truncsipsi2_d2_d2,@function
___truncsipsi2_d2_d2:
	add -4,a3
	mov d2,(a3)
	mov d3,(2,a3)
	movx (0,a3),d2
	add 4,a3
	rts

	.size ___truncsipsi2_d2_d2,.-___truncsipsi2_d2_d2
#endif

#ifdef  L_truncsipsi2_d2_d3
	.text
	.globl ___truncsipsi2_d2_d3
	.type  ___truncsipsi2_d2_d3,@function
___truncsipsi2_d2_d3:
	add -4,a3
	mov d2,(a3)
	mov d3,(2,a3)
	movx (0,a3),d3
	add 4,a3
	rts

	.size ___truncsipsi2_d2_d3,.-___truncsipsi2_d2_d3
#endif


#ifdef  L_cmpsi2
	.text
	.globl ___cmpsi2
	.type  ___cmpsi2,@function
___cmpsi2:
	add -4,a3
	mov a1,(0,a3)
        mov (10,a3),a1
        mov (8,a3),a0
        cmp a1,d1
        blt .L9
        bgt .L6
        cmp a0,d0
        bcc .L5
.L9:
        sub d0,d0
        jmp .L8
.L5:
        cmp a0,d0
        bhi .L6
        mov 1,d0
        jmp .L8
.L6:
        mov 2,d0
.L8:
	mov (0,a3),a1
	add 4,a3
        rts
	.size ___cmpsi2,.-___cmpsi2
#endif

#ifdef  L_ucmpsi2
	.text
	.globl ___ucmpsi2
	.type  ___ucmpsi2,@function
___ucmpsi2:
	add -4,a3
	mov a1,(0,a3)
        mov (10,a3),a1
        mov (8,a3),a0
        cmp a1,d1
        bcs .L9
        bhi .L6
        cmp a0,d0
        bcc .L5
.L9:
        sub d0,d0
        jmp .L8
.L5:
        cmp a0,d0
        bhi .L6
        mov 1,d0
        jmp .L8
.L6:
        mov 2,d0
.L8:
	mov (0,a3),a1
	add 4,a3
        rts
	.size ___ucmpsi2,.-___ucmpsi2
#endif


#ifdef L_prologue
	.text
	.globl ___prologue
	.type ___prologue,@function
___prologue:
	mov (0,a3),a0
	add -16,a3
	movx d2,(4,a3)
	movx d3,(8,a3)
	mov a1,(12,a3)
	mov a2,(16,a3)
	mov a0,(0,a3)
	rts
	.size ___prologue,.-___prologue
#endif

#ifdef L_epilogue_a0
	.text
	.globl ___epilogue_a0
	.type ___epilogue_a0,@function
___epilogue_a0:
	mov (0,a3),a0
	movx (4,a3),d2
	movx (8,a3),d3
	mov (12,a3),a1
	mov (16,a3),a2
	add 16,a3
	mov a0,(0,a3)
	rts
	.size ___epilogue_a0,.-___epilogue_a0
#endif

#ifdef L_epilogue_d0
	.text
	.globl ___epilogue_d0
	.type ___epilogue_d0,@function
___epilogue_d0:
	movx (0,a3),d0
	movx (4,a3),d2
	movx (8,a3),d3
	mov (12,a3),a1
	mov (16,a3),a2
	add 16,a3
	movx d0,(0,a3)
	rts
	.size ___epilogue_d0,.-___epilogue_d0
#endif

#ifdef L_epilogue_noreturn
	.text
	.globl ___epilogue_noreturn
	.type ___epilogue_noreturn,@function
___epilogue_noreturn:
	movx (0,a3),d2
	movx (4,a3),d3
	mov (8,a3),a1
	mov (12,a3),a2
	add 16,a3
	rts
	.size ___epilogue_noreturn,.-___epilogue_noreturn
#endif
