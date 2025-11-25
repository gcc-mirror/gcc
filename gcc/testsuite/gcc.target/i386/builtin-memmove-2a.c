/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic -minline-all-stringops" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { lp64 } } {^\t?\.} } } */

/*
**gcc_memmove_xmm:
**.LFB0:
**	.cfi_startproc
**	movq	%rdi, %rax
**	movq	%rsi, %rcx
**	cmpq	\$16, %rdx
**	jb	.L3
**	cmpq	\$32, %rdx
**	jbe	.L17
**	cmpq	\$128, %rdx
**	jbe	.L18
**	movq	%rdx, %rsi
**	cmpq	%rdi, %rcx
**	jb	.L11
**	je	.L2
**	movdqu	-16\(%rcx,%rdx\), %xmm7
**	movdqu	-32\(%rcx,%rdx\), %xmm6
**	movdqu	-48\(%rcx,%rdx\), %xmm5
**	movdqu	-64\(%rcx,%rdx\), %xmm4
**.L12:
**	movdqu	\(%rcx\), %xmm3
**	subq	\$64, %rsi
**	addq	\$64, %rdi
**	addq	\$64, %rcx
**	movdqu	-48\(%rcx\), %xmm2
**	movdqu	-32\(%rcx\), %xmm1
**	movdqu	-16\(%rcx\), %xmm0
**	movups	%xmm3, -64\(%rdi\)
**	movups	%xmm2, -48\(%rdi\)
**	movups	%xmm1, -32\(%rdi\)
**	movups	%xmm0, -16\(%rdi\)
**	cmpq	\$64, %rsi
**	ja	.L12
**	movups	%xmm7, -16\(%rax,%rdx\)
**	movups	%xmm6, -32\(%rax,%rdx\)
**	movups	%xmm5, -48\(%rax,%rdx\)
**	movups	%xmm4, -64\(%rax,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L3:
**	cmpq	\$8, %rdx
**	jb	.L19
**	movq	\(%rsi\), %rdi
**	movq	-8\(%rsi,%rdx\), %rcx
**	movq	%rdi, \(%rax\)
**	movq	%rcx, -8\(%rax,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L19:
**	cmpq	\$4, %rdx
**	jnb	.L6
**	cmpq	\$1, %rdx
**	ja	.L7
**	jb	.L2
**	movzbl	\(%rsi\), %edx
**	movb	%dl, \(%rdi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L17:
**	movdqu	\(%rsi\), %xmm1
**	movdqu	-16\(%rsi,%rdx\), %xmm0
**	movups	%xmm1, \(%rdi\)
**	movups	%xmm0, -16\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L18:
**	cmpq	\$64, %rdx
**	jb	.L10
**	movdqu	\(%rsi\), %xmm7
**	movdqu	16\(%rsi\), %xmm6
**	movdqu	32\(%rsi\), %xmm5
**	movdqu	48\(%rsi\), %xmm4
**	movdqu	-16\(%rsi,%rdx\), %xmm3
**	movdqu	-32\(%rsi,%rdx\), %xmm2
**	movdqu	-48\(%rsi,%rdx\), %xmm1
**	movdqu	-64\(%rsi,%rdx\), %xmm0
**	movups	%xmm7, \(%rdi\)
**	movups	%xmm6, 16\(%rdi\)
**	movups	%xmm5, 32\(%rdi\)
**	movups	%xmm4, 48\(%rdi\)
**	movups	%xmm3, -16\(%rdi,%rdx\)
**	movups	%xmm2, -32\(%rdi,%rdx\)
**	movups	%xmm1, -48\(%rdi,%rdx\)
**	movups	%xmm0, -64\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L6:
**	movl	\(%rsi\), %edi
**	movl	-4\(%rsi,%rdx\), %ecx
**	movl	%edi, \(%rax\)
**	movl	%ecx, -4\(%rax,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L11:
**	movdqu	\(%rcx\), %xmm7
**	movdqu	16\(%rcx\), %xmm6
**	leaq	\(%rdi,%rdx\), %rdi
**	movdqu	32\(%rcx\), %xmm5
**	movdqu	48\(%rcx\), %xmm4
**	addq	%rdx, %rcx
**.L13:
**	movdqu	-16\(%rcx\), %xmm3
**	movdqu	-32\(%rcx\), %xmm2
**	subq	\$64, %rsi
**	subq	\$64, %rdi
**	movdqu	-48\(%rcx\), %xmm1
**	movdqu	-64\(%rcx\), %xmm0
**	subq	\$64, %rcx
**	movups	%xmm3, 48\(%rdi\)
**	movups	%xmm2, 32\(%rdi\)
**	movups	%xmm1, 16\(%rdi\)
**	movups	%xmm0, \(%rdi\)
**	cmpq	\$64, %rsi
**	ja	.L13
**	movups	%xmm7, \(%rax\)
**	movups	%xmm6, 16\(%rax\)
**	movups	%xmm5, 32\(%rax\)
**	movups	%xmm4, 48\(%rax\)
**.L2:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L10:
**	movdqu	\(%rsi\), %xmm3
**	movdqu	16\(%rsi\), %xmm2
**	movdqu	-16\(%rsi,%rdx\), %xmm1
**	movdqu	-32\(%rsi,%rdx\), %xmm0
**	movups	%xmm3, \(%rdi\)
**	movups	%xmm2, 16\(%rdi\)
**	movups	%xmm1, -16\(%rdi,%rdx\)
**	movups	%xmm0, -32\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L7:
**	movzwl	\(%rsi\), %edi
**	movzwl	-2\(%rsi,%rdx\), %ecx
**	movw	%di, \(%rax\)
**	movw	%cx, -2\(%rax,%rdx\)
**	ret
**	.cfi_endproc
**...
*/

#ifndef gcc_memmove
#define gcc_memmove gcc_memmove_xmm
#endif

void *
gcc_memmove (void *a, void *b, __SIZE_TYPE__ n)
{
  return __builtin_memmove (a, b, n);
}
