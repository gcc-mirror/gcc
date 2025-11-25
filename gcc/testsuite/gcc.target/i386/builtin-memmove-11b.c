/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx512f -march=x86-64-v3 -mtune=generic -minline-all-stringops" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { lp64 } } {^\t?\.} } } */

/*
**gcc_memmove_ymm:
**.LFB0:
**	.cfi_startproc
**	movq	%rdi, %rax
**	movl	\$512, %edx
**	cmpq	%rdi, %rsi
**	jb	.L5
**	je	.L10
**	vmovdqu	480\(%rsi\), %ymm7
**	vmovdqu	448\(%rsi\), %ymm6
**	vmovdqu	416\(%rsi\), %ymm5
**	vmovdqu	384\(%rsi\), %ymm4
**.L6:
**	vmovdqu	\(%rsi\), %ymm3
**	vmovdqu	32\(%rsi\), %ymm2
**	addl	\$-128, %edx
**	subq	\$-128, %rax
**	vmovdqu	64\(%rsi\), %ymm1
**	vmovdqu	96\(%rsi\), %ymm0
**	subq	\$-128, %rsi
**	vmovdqu	%ymm3, -128\(%rax\)
**	vmovdqu	%ymm2, -96\(%rax\)
**	vmovdqu	%ymm1, -64\(%rax\)
**	vmovdqu	%ymm0, -32\(%rax\)
**	cmpl	\$128, %edx
**	ja	.L6
**	vmovdqu	%ymm7, 480\(%rdi\)
**	vmovdqu	%ymm6, 448\(%rdi\)
**	vmovdqu	%ymm5, 416\(%rdi\)
**	vmovdqu	%ymm4, 384\(%rdi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L5:
**	vmovdqu	\(%rsi\), %ymm7
**	vmovdqu	32\(%rsi\), %ymm6
**	leaq	512\(%rdi\), %rax
**	addq	\$512, %rsi
**	vmovdqu	-448\(%rsi\), %ymm5
**	vmovdqu	-416\(%rsi\), %ymm4
**.L7:
**	vmovdqu	-32\(%rsi\), %ymm3
**	addl	\$-128, %edx
**	addq	\$-128, %rax
**	addq	\$-128, %rsi
**	vmovdqu	64\(%rsi\), %ymm2
**	vmovdqu	32\(%rsi\), %ymm1
**	vmovdqu	\(%rsi\), %ymm0
**	vmovdqu	%ymm3, 96\(%rax\)
**	vmovdqu	%ymm2, 64\(%rax\)
**	vmovdqu	%ymm1, 32\(%rax\)
**	vmovdqu	%ymm0, \(%rax\)
**	cmpl	\$128, %edx
**	ja	.L7
**	vmovdqu	%ymm7, \(%rdi\)
**	vmovdqu	%ymm6, 32\(%rdi\)
**	vmovdqu	%ymm5, 64\(%rdi\)
**	vmovdqu	%ymm4, 96\(%rdi\)
**	vzeroupper
**.L10:
**	ret
**	.cfi_endproc
**...
*/

#define gcc_memmove gcc_memmove_ymm
#include "builtin-memmove-11a.c"
