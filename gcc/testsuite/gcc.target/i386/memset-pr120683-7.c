/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign -minline-all-stringops" } */
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
**	movq	%rsi, %xmm0
**	punpcklqdq	%xmm0, %xmm0
**	cmpq	\$64, %rdx
**	jnb	.L2
**	testb	\$32, %dl
**	jne	.L19
**	testb	\$16, %dl
**	jne	.L20
**	testb	\$8, %dl
**	jne	.L21
**	testb	\$4, %dl
**	jne	.L22
**	testq	%rdx, %rdx
**	jne	.L23
**.L1:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L2:
**	movups	%xmm0, -64\(%rdi,%rdx\)
**	movups	%xmm0, -48\(%rdi,%rdx\)
**	movups	%xmm0, -32\(%rdi,%rdx\)
**	movups	%xmm0, -16\(%rdi,%rdx\)
**	subq	\$1, %rdx
**	cmpq	\$64, %rdx
**	jb	.L1
**	andq	\$-64, %rdx
**	xorl	%eax, %eax
**.L9:
**	movups	%xmm0, \(%rdi,%rax\)
**	movups	%xmm0, 16\(%rdi,%rax\)
**	movups	%xmm0, 32\(%rdi,%rax\)
**	movups	%xmm0, 48\(%rdi,%rax\)
**	addq	\$64, %rax
**	cmpq	%rdx, %rax
**	jb	.L9
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L23:
**	movb	%sil, \(%rdi\)
**	testb	\$2, %dl
**	je	.L1
**	movw	%si, -2\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L19:
**	movups	%xmm0, \(%rdi\)
**	movups	%xmm0, 16\(%rdi\)
**	movups	%xmm0, -32\(%rdi,%rdx\)
**	movups	%xmm0, -16\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L20:
**	movups	%xmm0, \(%rdi\)
**	movups	%xmm0, -16\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L21:
**	movq	%rsi, \(%rdi\)
**	movq	%rsi, -8\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L22:
**	movl	%esi, \(%rdi\)
**	movl	%esi, -4\(%rdi,%rdx\)
**	ret
**	.cfi_endproc
**...
*/

void
foo (char *dest, int c, __SIZE_TYPE__ n)
{
  __builtin_memset (dest, c, n);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
