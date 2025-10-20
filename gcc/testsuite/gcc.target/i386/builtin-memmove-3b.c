/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx512f -march=x86-64-v3 -mtune=generic -minline-all-stringops" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { lp64 } } {^\t?\.} } } */

/*
**gcc_memmove_ymm:
**.LFB0:
**	.cfi_startproc
**	cmpq	\$16, %rdx
**	ja	.L16
**.L14:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L16:
**	movq	%rdi, %rcx
**	movq	%rsi, %rax
**	cmpq	\$32, %rdx
**	jb	.L6
**	cmpq	\$64, %rdx
**	ja	.L5
**	vmovdqu	\(%rsi\), %ymm1
**	vmovdqu	-32\(%rsi,%rdx\), %ymm0
**	vmovdqu	%ymm1, \(%rdi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rdx\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L6:
**	vmovdqu	\(%rsi\), %xmm1
**	vmovdqu	-16\(%rsi,%rdx\), %xmm0
**	vmovdqu	%xmm1, \(%rdi\)
**	vmovdqu	%xmm0, -16\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L5:
**	cmpq	\$256, %rdx
**	jbe	.L17
**	movq	%rdx, %rsi
**	cmpq	%rdi, %rax
**	jb	.L9
**	je	.L14
**	vmovdqu	-32\(%rax,%rdx\), %ymm7
**	vmovdqu	-64\(%rax,%rdx\), %ymm6
**	vmovdqu	-96\(%rax,%rdx\), %ymm5
**	vmovdqu	-128\(%rax,%rdx\), %ymm4
**.L10:
**	vmovdqu	\(%rax\), %ymm3
**	addq	\$-128, %rsi
**	subq	\$-128, %rcx
**	subq	\$-128, %rax
**	vmovdqu	-96\(%rax\), %ymm2
**	vmovdqu	-64\(%rax\), %ymm1
**	vmovdqu	-32\(%rax\), %ymm0
**	vmovdqu	%ymm3, -128\(%rcx\)
**	vmovdqu	%ymm2, -96\(%rcx\)
**	vmovdqu	%ymm1, -64\(%rcx\)
**	vmovdqu	%ymm0, -32\(%rcx\)
**	cmpq	\$128, %rsi
**	ja	.L10
**	vmovdqu	%ymm7, -32\(%rdi,%rdx\)
**	vmovdqu	%ymm6, -64\(%rdi,%rdx\)
**	vmovdqu	%ymm5, -96\(%rdi,%rdx\)
**	vmovdqu	%ymm4, -128\(%rdi,%rdx\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L17:
**	cmpq	\$128, %rdx
**	jb	.L8
**	vmovdqu	\(%rsi\), %ymm7
**	vmovdqu	32\(%rsi\), %ymm6
**	vmovdqu	64\(%rsi\), %ymm5
**	vmovdqu	96\(%rsi\), %ymm4
**	vmovdqu	-32\(%rsi,%rdx\), %ymm3
**	vmovdqu	-64\(%rsi,%rdx\), %ymm2
**	vmovdqu	-96\(%rsi,%rdx\), %ymm1
**	vmovdqu	-128\(%rsi,%rdx\), %ymm0
**	vmovdqu	%ymm7, \(%rdi\)
**	vmovdqu	%ymm6, 32\(%rdi\)
**	vmovdqu	%ymm5, 64\(%rdi\)
**	vmovdqu	%ymm4, 96\(%rdi\)
**	vmovdqu	%ymm3, -32\(%rdi,%rdx\)
**	vmovdqu	%ymm2, -64\(%rdi,%rdx\)
**	vmovdqu	%ymm1, -96\(%rdi,%rdx\)
**	vmovdqu	%ymm0, -128\(%rdi,%rdx\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L8:
**	vmovdqu	\(%rsi\), %ymm3
**	vmovdqu	32\(%rsi\), %ymm2
**	vmovdqu	-32\(%rsi,%rdx\), %ymm1
**	vmovdqu	-64\(%rsi,%rdx\), %ymm0
**	vmovdqu	%ymm3, \(%rdi\)
**	vmovdqu	%ymm2, 32\(%rdi\)
**	vmovdqu	%ymm1, -32\(%rdi,%rdx\)
**	vmovdqu	%ymm0, -64\(%rdi,%rdx\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L9:
**	vmovdqu	\(%rax\), %ymm3
**	vmovdqu	32\(%rax\), %ymm2
**	leaq	\(%rdi,%rdx\), %rcx
**	vmovdqu	64\(%rax\), %ymm1
**	vmovdqu	96\(%rax\), %ymm0
**	addq	%rdx, %rax
**.L11:
**	vmovdqu	-32\(%rax\), %ymm7
**	vmovdqu	-64\(%rax\), %ymm6
**	addq	\$-128, %rsi
**	addq	\$-128, %rcx
**	vmovdqu	-96\(%rax\), %ymm5
**	vmovdqu	-128\(%rax\), %ymm4
**	addq	\$-128, %rax
**	vmovdqu	%ymm7, 96\(%rcx\)
**	vmovdqu	%ymm6, 64\(%rcx\)
**	vmovdqu	%ymm5, 32\(%rcx\)
**	vmovdqu	%ymm4, \(%rcx\)
**	cmpq	\$128, %rsi
**	ja	.L11
**	vmovdqu	%ymm3, \(%rdi\)
**	vmovdqu	%ymm2, 32\(%rdi\)
**	vmovdqu	%ymm1, 64\(%rdi\)
**	vmovdqu	%ymm0, 96\(%rdi\)
**	vzeroupper
**	ret
**	.cfi_endproc
**...
*/

#define gcc_memmove gcc_memmove_ymm
#include "builtin-memmove-3a.c"
