/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign -minline-all-stringops" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	movabsq	\$289360691352306692, %rax
**	vpbroadcastq	%rax, %zmm0
**	cmpq	\$256, %rsi
**	jnb	.L2
**	testb	\$-128, %sil
**	jne	.L22
**	testb	\$64, %sil
**	jne	.L23
**	testb	\$32, %sil
**	jne	.L24
**	testb	\$16, %sil
**	jne	.L25
**	testb	\$8, %sil
**	jne	.L26
**	testb	\$4, %sil
**	jne	.L27
**	testq	%rsi, %rsi
**	jne	.L28
**.L20:
**	vzeroupper
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
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L28:
**	movb	\$4, \(%rdi\)
**	testb	\$2, %sil
**	je	.L20
**	movl	\$1028, %eax
**	movw	%ax, -2\(%rdi,%rsi\)
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L22:
**	vmovdqu64	%zmm0, \(%rdi\)
**	vmovdqu64	%zmm0, 64\(%rdi\)
**	vmovdqu64	%zmm0, -128\(%rdi,%rsi\)
**	vmovdqu64	%zmm0, -64\(%rdi,%rsi\)
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L23:
**	vmovdqu64	%zmm0, \(%rdi\)
**	vmovdqu64	%zmm0, -64\(%rdi,%rsi\)
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L24:
**	vmovdqu	%ymm0, \(%rdi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rsi\)
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L25:
**	vmovdqu	%xmm0, \(%rdi\)
**	vmovdqu	%xmm0, -16\(%rdi,%rsi\)
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L26:
**	movq	%rax, \(%rdi\)
**	movq	%rax, -8\(%rdi,%rsi\)
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L27:
**	movl	\$67372036, \(%rdi\)
**	movl	\$67372036, -4\(%rdi,%rsi\)
**	jmp	.L20
**	.cfi_endproc
**...
*/

void
foo (char *dest, __SIZE_TYPE__ n)
{
  __builtin_memset (dest, 4, n);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
