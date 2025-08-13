/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -fno-pic -mtune=generic -msse2 -mno-avx -mno-mmx -mno-80387 -mapxf -mtune-ctrl=prologue_using_move,epilogue_using_move" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/* end must be empty.  */

/*
**end:
**.LFB[0-9]+:
**	.cfi_startproc
**	ret
**	.cfi_endproc
**...
*/

/* inc doesn't have any callee saved registers.  */

/*
**inc:
**.LFB[0-9]+:
**	.cfi_startproc
**	addl	\$1, accumulator\(%rip\)
**	movq	\(%rdi\), %rax
**	addq	\$8, %rdi
**	jmp	\*%rax
**	.cfi_endproc
**...
*/

/* dec doesn't have any callee saved registers.  */

/*
**dec:
**.LFB[0-9]+:
**	.cfi_startproc
**	subl	\$1, accumulator\(%rip\)
**	movq	\(%rdi\), %rax
**	addq	\$8, %rdi
**	jmp	\*%rax
**	.cfi_endproc
**...
*/

/* start must save and restore all caller saved registers.  */

/*
**start:
**.LFB[0-9]+:
**	.cfi_startproc
**	subq	\$504, %rsp
**...
**	movq	%rax, 264\(%rsp\)
**	movq	%rdx, 272\(%rsp\)
**	movq	%rcx, 280\(%rsp\)
**	movq	%rbx, 288\(%rsp\)
**	movq	%rsi, 296\(%rsp\)
**	movq	%rdi, 304\(%rsp\)
**...
**	movl	\$code\+8, %edi
**	movq	%r8, 312\(%rsp\)
**	movq	%r9, 320\(%rsp\)
**	movq	%r10, 328\(%rsp\)
**	movq	%r11, 336\(%rsp\)
**	movq	%r12, 344\(%rsp\)
**	movq	%r13, 352\(%rsp\)
**	movq	%r14, 360\(%rsp\)
**	movq	%r15, 368\(%rsp\)
**	movq	%r16, 376\(%rsp\)
**	movq	%r17, 384\(%rsp\)
**	movq	%r18, 392\(%rsp\)
**	movq	%r19, 400\(%rsp\)
**	movq	%r20, 408\(%rsp\)
**	movq	%r21, 416\(%rsp\)
**	movq	%r22, 424\(%rsp\)
**	movq	%r23, 432\(%rsp\)
**	movq	%r24, 440\(%rsp\)
**	movq	%r25, 448\(%rsp\)
**	movq	%r26, 456\(%rsp\)
**	movq	%r27, 464\(%rsp\)
**	movq	%r28, 472\(%rsp\)
**	movq	%r29, 480\(%rsp\)
**	movq	%r30, 488\(%rsp\)
**	movq	%r31, 496\(%rsp\)
**...
**	movaps	%xmm0, \(%rsp\)
**	movaps	%xmm1, 16\(%rsp\)
**	movaps	%xmm2, 32\(%rsp\)
**	movaps	%xmm3, 48\(%rsp\)
**	movaps	%xmm4, 64\(%rsp\)
**	movaps	%xmm5, 80\(%rsp\)
**	movaps	%xmm6, 96\(%rsp\)
**	movaps	%xmm7, 112\(%rsp\)
**	movaps	%xmm8, 128\(%rsp\)
**	movaps	%xmm9, 144\(%rsp\)
**	movaps	%xmm10, 160\(%rsp\)
**	movaps	%xmm11, 176\(%rsp\)
**	movaps	%xmm12, 192\(%rsp\)
**	movaps	%xmm13, 208\(%rsp\)
**	movaps	%xmm14, 224\(%rsp\)
**	movaps	%xmm15, 240\(%rsp\)
**...
**	call	\*code\(%rip\)
**	movaps	\(%rsp\), %xmm0
**	movaps	16\(%rsp\), %xmm1
**	movaps	32\(%rsp\), %xmm2
**	movaps	48\(%rsp\), %xmm3
**	movaps	64\(%rsp\), %xmm4
**	movaps	80\(%rsp\), %xmm5
**	movaps	96\(%rsp\), %xmm6
**	movaps	112\(%rsp\), %xmm7
**	movaps	128\(%rsp\), %xmm8
**	movaps	144\(%rsp\), %xmm9
**	movaps	160\(%rsp\), %xmm10
**	movaps	176\(%rsp\), %xmm11
**	movaps	192\(%rsp\), %xmm12
**	movaps	208\(%rsp\), %xmm13
**	movq	264\(%rsp\), %rax
**	movq	272\(%rsp\), %rdx
**	movq	280\(%rsp\), %rcx
**	movq	288\(%rsp\), %rbx
**	movq	296\(%rsp\), %rsi
**	movq	304\(%rsp\), %rdi
**	movq	312\(%rsp\), %r8
**	movq	320\(%rsp\), %r9
**	movq	328\(%rsp\), %r10
**	movq	336\(%rsp\), %r11
**	movq	344\(%rsp\), %r12
**	movq	352\(%rsp\), %r13
**	movq	360\(%rsp\), %r14
**	movq	368\(%rsp\), %r15
**	movq	376\(%rsp\), %r16
**	movq	384\(%rsp\), %r17
**	movaps	224\(%rsp\), %xmm14
**	movaps	240\(%rsp\), %xmm15
**	movq	392\(%rsp\), %r18
**	movq	400\(%rsp\), %r19
**	movq	408\(%rsp\), %r20
**	movq	416\(%rsp\), %r21
**	movq	424\(%rsp\), %r22
**	movq	432\(%rsp\), %r23
**	movq	440\(%rsp\), %r24
**	movq	448\(%rsp\), %r25
**	movq	456\(%rsp\), %r26
**	movq	464\(%rsp\), %r27
**	movq	472\(%rsp\), %r28
**	movq	480\(%rsp\), %r29
**	movq	488\(%rsp\), %r30
**	movq	496\(%rsp\), %r31
**	addq	\$504, %rsp
**...
**	ret
**	.cfi_endproc
**...
*/

#include "no-callee-saved-19a.c"
