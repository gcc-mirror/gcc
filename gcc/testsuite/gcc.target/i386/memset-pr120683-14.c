/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -fasynchronous-unwind-tables -fdwarf2-cfi-asm -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign -minline-all-stringops" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	pxor	%xmm0, %xmm0
**	cmpq	\$64, %rsi
**	jnb	.L2
**	testb	\$32, %sil
**	jne	.L19
**	testb	\$16, %sil
**	jne	.L20
**	testb	\$8, %sil
**	jne	.L21
**	testb	\$4, %sil
**	jne	.L22
**	testq	%rsi, %rsi
**	jne	.L23
**.L1:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L2:
**	movups	%xmm0, -64\(%rdi,%rsi\)
**	movups	%xmm0, -48\(%rdi,%rsi\)
**	movups	%xmm0, -32\(%rdi,%rsi\)
**	movups	%xmm0, -16\(%rdi,%rsi\)
**	subq	\$1, %rsi
**	cmpq	\$64, %rsi
**	jb	.L1
**	andq	\$-64, %rsi
**	xorl	%eax, %eax
**.L9:
**	movups	%xmm0, \(%rdi,%rax\)
**	movups	%xmm0, 16\(%rdi,%rax\)
**	movups	%xmm0, 32\(%rdi,%rax\)
**	movups	%xmm0, 48\(%rdi,%rax\)
**	addq	\$64, %rax
**	cmpq	%rsi, %rax
**	jb	.L9
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L23:
**	movb	\$0, \(%rdi\)
**	testb	\$2, %sil
**	je	.L1
**	xorl	%eax, %eax
**	movw	%ax, -2\(%rdi,%rsi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L19:
**	movups	%xmm0, \(%rdi\)
**	movups	%xmm0, 16\(%rdi\)
**	movups	%xmm0, -32\(%rdi,%rsi\)
**	movups	%xmm0, -16\(%rdi,%rsi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L20:
**	movups	%xmm0, \(%rdi\)
**	movups	%xmm0, -16\(%rdi,%rsi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L21:
**	movq	\$0, \(%rdi\)
**	movq	\$0, -8\(%rdi,%rsi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L22:
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
