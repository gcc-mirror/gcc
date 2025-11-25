/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -mmove-max=512 -mtune=generic -minline-all-stringops" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { lp64 } } {^\t?\.} } } */

/*
**gcc_memmove_zmm:
**.LFB0:
**	.cfi_startproc
**	movq	%rdi, %rax
**	movq	%rsi, %rcx
**	cmpq	\$64, %rdx
**	jb	.L3
**	cmpq	\$128, %rdx
**	jbe	.L19
**	cmpq	\$512, %rdx
**	jbe	.L20
**	movq	%rdx, %rsi
**	cmpq	%rdi, %rcx
**	jb	.L13
**	je	.L2
**	vmovdqu64	-64\(%rcx,%rdx\), %zmm7
**	vmovdqu64	-128\(%rcx,%rdx\), %zmm6
**	vmovdqu64	-192\(%rcx,%rdx\), %zmm5
**	vmovdqu64	-256\(%rcx,%rdx\), %zmm4
**.L14:
**	vmovdqu64	\(%rcx\), %zmm3
**	vmovdqu64	64\(%rcx\), %zmm2
**	subq	\$256, %rsi
**	addq	\$256, %rdi
**	vmovdqu64	128\(%rcx\), %zmm1
**	addq	\$256, %rcx
**	vmovdqu64	-64\(%rcx\), %zmm0
**	vmovdqu64	%zmm3, -256\(%rdi\)
**	vmovdqu64	%zmm2, -192\(%rdi\)
**	vmovdqu64	%zmm1, -128\(%rdi\)
**	vmovdqu64	%zmm0, -64\(%rdi\)
**	cmpq	\$256, %rsi
**	ja	.L14
**	vmovdqu64	%zmm7, -64\(%rax,%rdx\)
**	vmovdqu64	%zmm6, -128\(%rax,%rdx\)
**	vmovdqu64	%zmm5, -192\(%rax,%rdx\)
**	vmovdqu64	%zmm4, -256\(%rax,%rdx\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L3:
**	cmpq	\$32, %rdx
**	jb	.L21
**	vmovdqu	\(%rsi\), %ymm1
**	vmovdqu	-32\(%rsi,%rdx\), %ymm0
**	vmovdqu	%ymm1, \(%rdi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rdx\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L21:
**	cmpq	\$16, %rdx
**	jnb	.L6
**	cmpq	\$8, %rdx
**	jnb	.L7
**	cmpq	\$4, %rdx
**	jnb	.L8
**	cmpq	\$1, %rdx
**	ja	.L9
**	jb	.L2
**	movzbl	\(%rsi\), %edx
**	movb	%dl, \(%rdi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L19:
**	vmovdqu64	\(%rsi\), %zmm1
**	vmovdqu64	-64\(%rsi,%rdx\), %zmm0
**	vmovdqu64	%zmm1, \(%rdi\)
**	vmovdqu64	%zmm0, -64\(%rdi,%rdx\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L20:
**	cmpq	\$256, %rdx
**	jb	.L12
**	vmovdqu64	\(%rsi\), %zmm7
**	vmovdqu64	64\(%rsi\), %zmm6
**	vmovdqu64	-64\(%rsi,%rdx\), %zmm3
**	vmovdqu64	-128\(%rsi,%rdx\), %zmm2
**	vmovdqu64	128\(%rsi\), %zmm5
**	vmovdqu64	192\(%rsi\), %zmm4
**	vmovdqu64	-192\(%rsi,%rdx\), %zmm1
**	vmovdqu64	-256\(%rsi,%rdx\), %zmm0
**	vmovdqu64	%zmm7, \(%rdi\)
**	vmovdqu64	%zmm6, 64\(%rdi\)
**	vmovdqu64	%zmm5, 128\(%rdi\)
**	vmovdqu64	%zmm4, 192\(%rdi\)
**	vmovdqu64	%zmm3, -64\(%rdi,%rdx\)
**	vmovdqu64	%zmm2, -128\(%rdi,%rdx\)
**	vmovdqu64	%zmm1, -192\(%rdi,%rdx\)
**	vmovdqu64	%zmm0, -256\(%rdi,%rdx\)
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
**.L13:
**	vmovdqu64	\(%rcx\), %zmm7
**	leaq	\(%rdi,%rdx\), %rdi
**	vmovdqu64	64\(%rcx\), %zmm6
**	vmovdqu64	128\(%rcx\), %zmm5
**	vmovdqu64	192\(%rcx\), %zmm4
**	addq	%rdx, %rcx
**.L15:
**	vmovdqu64	-64\(%rcx\), %zmm3
**	vmovdqu64	-128\(%rcx\), %zmm2
**	subq	\$256, %rsi
**	subq	\$256, %rdi
**	vmovdqu64	-192\(%rcx\), %zmm1
**	subq	\$256, %rcx
**	vmovdqu64	\(%rcx\), %zmm0
**	vmovdqu64	%zmm3, 192\(%rdi\)
**	vmovdqu64	%zmm2, 128\(%rdi\)
**	vmovdqu64	%zmm1, 64\(%rdi\)
**	vmovdqu64	%zmm0, \(%rdi\)
**	cmpq	\$256, %rsi
**	ja	.L15
**	vmovdqu64	%zmm7, \(%rax\)
**	vmovdqu64	%zmm6, 64\(%rax\)
**	vmovdqu64	%zmm5, 128\(%rax\)
**	vmovdqu64	%zmm4, 192\(%rax\)
**	vzeroupper
**.L2:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L12:
**	vmovdqu64	\(%rsi\), %zmm3
**	vmovdqu64	64\(%rsi\), %zmm2
**	vmovdqu64	-64\(%rsi,%rdx\), %zmm1
**	vmovdqu64	-128\(%rsi,%rdx\), %zmm0
**	vmovdqu64	%zmm3, \(%rdi\)
**	vmovdqu64	%zmm2, 64\(%rdi\)
**	vmovdqu64	%zmm1, -64\(%rdi,%rdx\)
**	vmovdqu64	%zmm0, -128\(%rdi,%rdx\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L7:
**	movq	\(%rsi\), %rdi
**	movq	-8\(%rsi,%rdx\), %rcx
**	movq	%rdi, \(%rax\)
**	movq	%rcx, -8\(%rax,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L8:
**	movl	\(%rsi\), %edi
**	movl	-4\(%rsi,%rdx\), %ecx
**	movl	%edi, \(%rax\)
**	movl	%ecx, -4\(%rax,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L9:
**	movzwl	\(%rsi\), %edi
**	movzwl	-2\(%rsi,%rdx\), %ecx
**	movw	%di, \(%rax\)
**	movw	%cx, -2\(%rax,%rdx\)
**	ret
**	.cfi_endproc
**...
*/

#define gcc_memmove gcc_memmove_zmm
#include "builtin-memmove-2a.c"
