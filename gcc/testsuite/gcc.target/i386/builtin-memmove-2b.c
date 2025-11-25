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
**	movq	%rsi, %rcx
**	cmpq	\$32, %rdx
**	jb	.L3
**	cmpq	\$64, %rdx
**	jbe	.L18
**	cmpq	\$256, %rdx
**	jbe	.L19
**	movq	%rdx, %rsi
**	cmpq	%rdi, %rcx
**	jb	.L12
**	je	.L2
**	vmovdqu	-32\(%rcx,%rdx\), %ymm7
**	vmovdqu	-64\(%rcx,%rdx\), %ymm6
**	vmovdqu	-96\(%rcx,%rdx\), %ymm5
**	vmovdqu	-128\(%rcx,%rdx\), %ymm4
**.L13:
**	vmovdqu	\(%rcx\), %ymm3
**	addq	\$-128, %rsi
**	subq	\$-128, %rdi
**	subq	\$-128, %rcx
**	vmovdqu	-96\(%rcx\), %ymm2
**	vmovdqu	-64\(%rcx\), %ymm1
**	vmovdqu	-32\(%rcx\), %ymm0
**	vmovdqu	%ymm3, -128\(%rdi\)
**	vmovdqu	%ymm2, -96\(%rdi\)
**	vmovdqu	%ymm1, -64\(%rdi\)
**	vmovdqu	%ymm0, -32\(%rdi\)
**	cmpq	\$128, %rsi
**	ja	.L13
**	vmovdqu	%ymm7, -32\(%rax,%rdx\)
**	vmovdqu	%ymm6, -64\(%rax,%rdx\)
**	vmovdqu	%ymm5, -96\(%rax,%rdx\)
**	vmovdqu	%ymm4, -128\(%rax,%rdx\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L3:
**	cmpq	\$16, %rdx
**	jb	.L20
**	vmovdqu	\(%rsi\), %xmm1
**	vmovdqu	-16\(%rsi,%rdx\), %xmm0
**	vmovdqu	%xmm1, \(%rdi\)
**	vmovdqu	%xmm0, -16\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L20:
**	cmpq	\$8, %rdx
**	jnb	.L6
**	cmpq	\$4, %rdx
**	jnb	.L7
**	cmpq	\$1, %rdx
**	ja	.L8
**	jb	.L2
**	movzbl	\(%rsi\), %edx
**	movb	%dl, \(%rdi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L18:
**	vmovdqu	\(%rsi\), %ymm1
**	vmovdqu	-32\(%rsi,%rdx\), %ymm0
**	vmovdqu	%ymm1, \(%rdi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rdx\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L19:
**	cmpq	\$128, %rdx
**	jb	.L11
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
**.L6:
**	movq	\(%rsi\), %rdi
**	movq	-8\(%rsi,%rdx\), %rcx
**	movq	%rdi, \(%rax\)
**	movq	%rcx, -8\(%rax,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L12:
**	vmovdqu	\(%rcx\), %ymm7
**	vmovdqu	32\(%rcx\), %ymm6
**	leaq	\(%rdi,%rdx\), %rdi
**	vmovdqu	64\(%rcx\), %ymm5
**	vmovdqu	96\(%rcx\), %ymm4
**	addq	%rdx, %rcx
**.L14:
**	vmovdqu	-32\(%rcx\), %ymm3
**	vmovdqu	-64\(%rcx\), %ymm2
**	addq	\$-128, %rsi
**	addq	\$-128, %rdi
**	vmovdqu	-96\(%rcx\), %ymm1
**	vmovdqu	-128\(%rcx\), %ymm0
**	addq	\$-128, %rcx
**	vmovdqu	%ymm3, 96\(%rdi\)
**	vmovdqu	%ymm2, 64\(%rdi\)
**	vmovdqu	%ymm1, 32\(%rdi\)
**	vmovdqu	%ymm0, \(%rdi\)
**	cmpq	\$128, %rsi
**	ja	.L14
**	vmovdqu	%ymm7, \(%rax\)
**	vmovdqu	%ymm6, 32\(%rax\)
**	vmovdqu	%ymm5, 64\(%rax\)
**	vmovdqu	%ymm4, 96\(%rax\)
**	vzeroupper
**.L2:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L11:
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
**.L7:
**	movl	\(%rsi\), %edi
**	movl	-4\(%rsi,%rdx\), %ecx
**	movl	%edi, \(%rax\)
**	movl	%ecx, -4\(%rax,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L8:
**	movzwl	\(%rsi\), %edi
**	movzwl	-2\(%rsi,%rdx\), %ecx
**	movw	%di, \(%rax\)
**	movw	%cx, -2\(%rax,%rdx\)
**	ret
**	.cfi_endproc
**...
*/

#define gcc_memmove gcc_memmove_ymm
#include "builtin-memmove-2a.c"
