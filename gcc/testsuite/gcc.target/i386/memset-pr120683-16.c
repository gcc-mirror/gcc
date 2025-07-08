/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -fasynchronous-unwind-tables -fdwarf2-cfi-asm -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign -minline-all-stringops" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	vpxor	%xmm0, %xmm0, %xmm0
**	cmpq	\$256, %rsi
**	jnb	.L2
**	testb	\$-128, %sil
**	jne	.L23
**	testb	\$64, %sil
**	jne	.L24
**	testb	\$32, %sil
**	jne	.L25
**	testb	\$16, %sil
**	jne	.L26
**	testb	\$8, %sil
**	jne	.L27
**	testb	\$4, %sil
**	jne	.L28
**	testq	%rsi, %rsi
**	jne	.L29
**.L21:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L2:
**	vmovdqu64	%zmm0, -256\(%rdi,%rsi\)
**	vmovdqu64	%zmm0, -192\(%rdi,%rsi\)
**	vmovdqu64	%zmm0, -128\(%rdi,%rsi\)
**	vmovdqu64	%zmm0, -64\(%rdi,%rsi\)
**	subq	\$1, %rsi
**	cmpq	\$256, %rsi
**	jb	.L20
**	xorb	%sil, %sil
**	xorl	%eax, %eax
**.L11:
**	vmovdqu64	%zmm0, \(%rdi,%rax\)
**	vmovdqu64	%zmm0, 64\(%rdi,%rax\)
**	vmovdqu64	%zmm0, 128\(%rdi,%rax\)
**	vmovdqu64	%zmm0, 192\(%rdi,%rax\)
**	addq	\$256, %rax
**	cmpq	%rsi, %rax
**	jb	.L11
**.L20:
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L29:
**	movb	\$0, \(%rdi\)
**	testb	\$2, %sil
**	je	.L21
**	xorl	%eax, %eax
**	movw	%ax, -2\(%rdi,%rsi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L23:
**	vmovdqu64	%zmm0, \(%rdi\)
**	vmovdqu64	%zmm0, 64\(%rdi\)
**	vmovdqu64	%zmm0, -128\(%rdi,%rsi\)
**	vmovdqu64	%zmm0, -64\(%rdi,%rsi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L24:
**	vmovdqu64	%zmm0, \(%rdi\)
**	vmovdqu64	%zmm0, -64\(%rdi,%rsi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L25:
**	vmovdqu	%ymm0, \(%rdi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rsi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L26:
**	vmovdqu	%xmm0, \(%rdi\)
**	vmovdqu	%xmm0, -16\(%rdi,%rsi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L27:
**	movq	\$0, \(%rdi\)
**	movq	\$0, -8\(%rdi,%rsi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L28:
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
