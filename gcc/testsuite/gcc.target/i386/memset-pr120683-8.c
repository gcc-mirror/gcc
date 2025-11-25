/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign -minline-all-stringops" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	movabsq	\$72340172838076673, %rax
**	movzbl	%sil, %esi
**	imulq	%rax, %rsi
**	vmovq	%rsi, %xmm1
**	vpbroadcastq	%xmm1, %ymm0
**	cmpq	\$128, %rdx
**	jnb	.L2
**	testb	\$64, %dl
**	jne	.L21
**	testb	\$32, %dl
**	jne	.L22
**	testb	\$16, %dl
**	jne	.L23
**	testb	\$8, %dl
**	jne	.L24
**	testb	\$4, %dl
**	jne	.L25
**	testq	%rdx, %rdx
**	jne	.L26
**.L19:
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L2:
**	vmovdqu	%ymm0, -128\(%rdi,%rdx\)
**	vmovdqu	%ymm0, -96\(%rdi,%rdx\)
**	vmovdqu	%ymm0, -64\(%rdi,%rdx\)
**	vmovdqu	%ymm0, -32\(%rdi,%rdx\)
**	subq	\$1, %rdx
**	cmpq	\$128, %rdx
**	jb	.L19
**	andq	\$-128, %rdx
**	xorl	%eax, %eax
**.L10:
**	vmovdqu	%ymm0, \(%rdi,%rax\)
**	vmovdqu	%ymm0, 32\(%rdi,%rax\)
**	vmovdqu	%ymm0, 64\(%rdi,%rax\)
**	vmovdqu	%ymm0, 96\(%rdi,%rax\)
**	subq	\$-128, %rax
**	cmpq	%rdx, %rax
**	jb	.L10
**	jmp	.L19
**	.p2align 4,,10
**	.p2align 3
**.L26:
**	movb	%sil, \(%rdi\)
**	testb	\$2, %dl
**	je	.L19
**	movw	%si, -2\(%rdi,%rdx\)
**	jmp	.L19
**	.p2align 4,,10
**	.p2align 3
**.L21:
**	vmovdqu	%ymm0, \(%rdi\)
**	vmovdqu	%ymm0, 32\(%rdi\)
**	vmovdqu	%ymm0, -64\(%rdi,%rdx\)
**	vmovdqu	%ymm0, -32\(%rdi,%rdx\)
**	jmp	.L19
**	.p2align 4,,10
**	.p2align 3
**.L22:
**	vmovdqu	%ymm0, \(%rdi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rdx\)
**	jmp	.L19
**	.p2align 4,,10
**	.p2align 3
**.L23:
**	vmovdqu	%xmm0, \(%rdi\)
**	vmovdqu	%xmm0, -16\(%rdi,%rdx\)
**	jmp	.L19
**	.p2align 4,,10
**	.p2align 3
**.L24:
**	movq	%rsi, \(%rdi\)
**	movq	%rsi, -8\(%rdi,%rdx\)
**	jmp	.L19
**	.p2align 4,,10
**	.p2align 3
**.L25:
**	movl	%esi, \(%rdi\)
**	movl	%esi, -4\(%rdi,%rdx\)
**	jmp	.L19
**	.cfi_endproc
**...
*/

void
foo (char *dest, int c, __SIZE_TYPE__ n)
{
  __builtin_memset (dest, c, n);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
