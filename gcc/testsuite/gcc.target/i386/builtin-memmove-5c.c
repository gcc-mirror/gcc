/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -mmove-max=512 -mtune=generic -minline-all-stringops" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { lp64 } } {^\t?\.} } } */

/*
**gcc_memmove_zmm:
**.LFB0:
**	.cfi_startproc
**	cmpq	\$67, %rdx
**	ja	.L14
**.L12:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L14:
**	movq	%rdi, %rcx
**	movq	%rsi, %rax
**	cmpq	\$128, %rdx
**	jbe	.L15
**	cmpq	\$512, %rdx
**	ja	.L5
**	cmpq	\$256, %rdx
**	jnb	.L16
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
**.L15:
**	vmovdqu64	\(%rsi\), %zmm1
**	vmovdqu64	-64\(%rsi,%rdx\), %zmm0
**	vmovdqu64	%zmm1, \(%rdi\)
**	vmovdqu64	%zmm0, -64\(%rdi,%rdx\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L5:
**	movq	%rdx, %rsi
**	cmpq	%rdi, %rax
**	jb	.L7
**	je	.L12
**	vmovdqu64	-64\(%rax,%rdx\), %zmm7
**	vmovdqu64	-128\(%rax,%rdx\), %zmm6
**	vmovdqu64	-192\(%rax,%rdx\), %zmm5
**	vmovdqu64	-256\(%rax,%rdx\), %zmm4
**.L8:
**	vmovdqu64	\(%rax\), %zmm3
**	addq	\$256, %rax
**	vmovdqu64	-192\(%rax\), %zmm2
**	subq	\$256, %rsi
**	vmovdqu64	-128\(%rax\), %zmm1
**	vmovdqu64	-64\(%rax\), %zmm0
**	addq	\$256, %rcx
**	vmovdqu64	%zmm3, -256\(%rcx\)
**	vmovdqu64	%zmm2, -192\(%rcx\)
**	vmovdqu64	%zmm1, -128\(%rcx\)
**	vmovdqu64	%zmm0, -64\(%rcx\)
**	cmpq	\$256, %rsi
**	ja	.L8
**	vmovdqu64	%zmm7, -64\(%rdi,%rdx\)
**	vmovdqu64	%zmm6, -128\(%rdi,%rdx\)
**	vmovdqu64	%zmm5, -192\(%rdi,%rdx\)
**	vmovdqu64	%zmm4, -256\(%rdi,%rdx\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L7:
**	vmovdqu64	\(%rax\), %zmm3
**	leaq	\(%rdi,%rdx\), %rcx
**	vmovdqu64	64\(%rax\), %zmm2
**	vmovdqu64	128\(%rax\), %zmm1
**	vmovdqu64	192\(%rax\), %zmm0
**	addq	%rdx, %rax
**.L9:
**	vmovdqu64	-64\(%rax\), %zmm7
**	subq	\$256, %rax
**	vmovdqu64	128\(%rax\), %zmm6
**	subq	\$256, %rsi
**	vmovdqu64	64\(%rax\), %zmm5
**	vmovdqu64	\(%rax\), %zmm4
**	subq	\$256, %rcx
**	vmovdqu64	%zmm7, 192\(%rcx\)
**	vmovdqu64	%zmm6, 128\(%rcx\)
**	vmovdqu64	%zmm5, 64\(%rcx\)
**	vmovdqu64	%zmm4, \(%rcx\)
**	cmpq	\$256, %rsi
**	ja	.L9
**	vmovdqu64	%zmm3, \(%rdi\)
**	vmovdqu64	%zmm2, 64\(%rdi\)
**	vmovdqu64	%zmm1, 128\(%rdi\)
**	vmovdqu64	%zmm0, 192\(%rdi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L16:
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
**	.cfi_endproc
**...
*/

#define gcc_memmove gcc_memmove_zmm
#include "builtin-memmove-5a.c"
