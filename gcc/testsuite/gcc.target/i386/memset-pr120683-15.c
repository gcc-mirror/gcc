/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -fasynchronous-unwind-tables -fdwarf2-cfi-asm -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign -minline-all-stringops" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	vpxor	%xmm0, %xmm0, %xmm0
**	cmpq	\$128, %rsi
**	jnb	.L2
**	testb	\$64, %sil
**	jne	.L22
**	testb	\$32, %sil
**	jne	.L23
**	testb	\$16, %sil
**	jne	.L24
**	testb	\$8, %sil
**	jne	.L25
**	testb	\$4, %sil
**	jne	.L26
**	testq	%rsi, %rsi
**	jne	.L27
**.L20:
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
**.L19:
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L27:
**	movb	\$0, \(%rdi\)
**	testb	\$2, %sil
**	je	.L20
**	xorl	%eax, %eax
**	movw	%ax, -2\(%rdi,%rsi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L22:
**	vmovdqu	%ymm0, \(%rdi\)
**	vmovdqu	%ymm0, 32\(%rdi\)
**	vmovdqu	%ymm0, -64\(%rdi,%rsi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rsi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L23:
**	vmovdqu	%ymm0, \(%rdi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rsi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L24:
**	vmovdqu	%xmm0, \(%rdi\)
**	vmovdqu	%xmm0, -16\(%rdi,%rsi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L25:
**	movq	\$0, \(%rdi\)
**	movq	\$0, -8\(%rdi,%rsi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L26:
**	movl	\$0, \(%rdi\)
**	movl	\$0, -4\(%rdi,%rsi\)
**	ret
**	.cfi_endproc
**...
*/

void
foo (char *dest, __SIZE_TYPE__ n)
{
  __builtin_memset (dest, 0, n);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
