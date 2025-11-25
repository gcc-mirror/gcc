/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign -minline-all-stringops" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	movabsq	\$289360691352306692, %rax
**	vmovq	%rax, %xmm1
**	vpbroadcastq	%xmm1, %ymm0
**	cmpq	\$128, %rsi
**	jnb	.L2
**	testb	\$64, %sil
**	jne	.L21
**	testb	\$32, %sil
**	jne	.L22
**	testb	\$16, %sil
**	jne	.L23
**	testb	\$8, %sil
**	jne	.L24
**	testb	\$4, %sil
**	jne	.L25
**	testq	%rsi, %rsi
**	jne	.L26
**.L19:
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L2:
**	vmovdqu	%ymm0, -128\(%rdi,%rsi\)
**	vmovdqu	%ymm0, -96\(%rdi,%rsi\)
**	vmovdqu	%ymm0, -64\(%rdi,%rsi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rsi\)
**	subq	\$1, %rsi
**	cmpq	\$128, %rsi
**	jb	.L19
**	andq	\$-128, %rsi
**	xorl	%eax, %eax
**.L10:
**	vmovdqu	%ymm0, \(%rdi,%rax\)
**	vmovdqu	%ymm0, 32\(%rdi,%rax\)
**	vmovdqu	%ymm0, 64\(%rdi,%rax\)
**	vmovdqu	%ymm0, 96\(%rdi,%rax\)
**	subq	\$-128, %rax
**	cmpq	%rsi, %rax
**	jb	.L10
**	jmp	.L19
**	.p2align 4,,10
**	.p2align 3
**.L26:
**	movb	\$4, \(%rdi\)
**	testb	\$2, %sil
**	je	.L19
**	movl	\$1028, %eax
**	movw	%ax, -2\(%rdi,%rsi\)
**	jmp	.L19
**	.p2align 4,,10
**	.p2align 3
**.L21:
**	vmovdqu	%ymm0, \(%rdi\)
**	vmovdqu	%ymm0, 32\(%rdi\)
**	vmovdqu	%ymm0, -64\(%rdi,%rsi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rsi\)
**	jmp	.L19
**	.p2align 4,,10
**	.p2align 3
**.L22:
**	vmovdqu	%ymm0, \(%rdi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rsi\)
**	jmp	.L19
**	.p2align 4,,10
**	.p2align 3
**.L23:
**	vmovdqu	%xmm0, \(%rdi\)
**	vmovdqu	%xmm0, -16\(%rdi,%rsi\)
**	jmp	.L19
**	.p2align 4,,10
**	.p2align 3
**.L24:
**	movq	%rax, \(%rdi\)
**	movq	%rax, -8\(%rdi,%rsi\)
**	jmp	.L19
**	.p2align 4,,10
**	.p2align 3
**.L25:
**	movl	\$67372036, \(%rdi\)
**	movl	\$67372036, -4\(%rdi,%rsi\)
**	jmp	.L19
**	.cfi_endproc
**...
*/

void
foo (char *dest, __SIZE_TYPE__ n)
{
  __builtin_memset (dest, 4, n);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
