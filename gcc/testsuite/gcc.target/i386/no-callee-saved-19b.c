/* { dg-do compile { target { *-*-linux* && maybe_x32 } } } */
/* { dg-options "-O2 -mx32 -fno-pic -mtune=generic -msse2 -mno-avx -mno-mmx -mno-80387 -mno-apxf -mtune-ctrl=prologue_using_move,epilogue_using_move" } */
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
**	movq	%rdi, %rax
**	movl	\(%eax\), %eax
**	leal	4\(%rdi\), %edi
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
**	movq	%rdi, %rax
**	movl	\(%eax\), %eax
**	leal	4\(%rdi\), %edi
**	jmp	\*%rax
**	.cfi_endproc
**...
*/

/* start must save and restore all caller saved registers.  */

/*
**start:
**.LFB[0-9]+:
**	.cfi_startproc
**	subl	\$376, %esp
**...
**	movq	%rax, 256\(%rsp\)
**	movq	%rdx, 264\(%rsp\)
**	movq	%rcx, 272\(%rsp\)
**	movq	%rbx, 280\(%rsp\)
**	movq	%rsi, 288\(%rsp\)
**	movq	%rdi, 296\(%rsp\)
**...
**	movl	\$code\+4, %edi
**	movq	%rbp, 304\(%rsp\)
**	movq	%r8, 312\(%rsp\)
**	movq	%r9, 320\(%rsp\)
**	movq	%r10, 328\(%rsp\)
**	movq	%r11, 336\(%rsp\)
**	movq	%r12, 344\(%rsp\)
**	movq	%r13, 352\(%rsp\)
**	movq	%r14, 360\(%rsp\)
**	movq	%r15, 368\(%rsp\)
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
**	movl	code\(%rip\), %ebp
**	call	\*%rbp
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
**	movaps	224\(%rsp\), %xmm14
**	movaps	240\(%rsp\), %xmm15
**	movq	256\(%rsp\), %rax
**	movq	264\(%rsp\), %rdx
**	movq	272\(%rsp\), %rcx
**	movq	280\(%rsp\), %rbx
**	movq	288\(%rsp\), %rsi
**	movq	296\(%rsp\), %rdi
**	movq	304\(%rsp\), %rbp
**	movq	312\(%rsp\), %r8
**	movq	320\(%rsp\), %r9
**	movq	328\(%rsp\), %r10
**	movq	336\(%rsp\), %r11
**	movq	344\(%rsp\), %r12
**	movq	352\(%rsp\), %r13
**	movq	360\(%rsp\), %r14
**	movq	368\(%rsp\), %r15
**	addl	\$376, %esp
**...
**	ret
**	.cfi_endproc
**...
*/

#include "no-callee-saved-19a.c"
