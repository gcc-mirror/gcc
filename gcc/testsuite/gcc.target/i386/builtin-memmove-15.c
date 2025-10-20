/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic -minline-all-stringops" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { lp64 } } {^\t?\.} } } */

/*
**gcc_memmove:
**.LFB0:
**	.cfi_startproc
**	cmpq	\$66, %rdx
**	jbe	.L12
**.L1:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L12:
**	cmpl	\$16, %edx
**	jnb	.L13
**	cmpl	\$8, %edx
**	jnb	.L6
**	cmpl	\$4, %edx
**	jnb	.L7
**	cmpl	\$1, %edx
**	ja	.L8
**	jb	.L1
**	movzbl	\(%rsi\), %eax
**	movb	%al, \(%rdi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L13:
**	cmpl	\$32, %edx
**	ja	.L5
**	movl	%edx, %edx
**	movdqu	\(%rsi\), %xmm1
**	movdqu	-16\(%rsi,%rdx\), %xmm0
**	movups	%xmm1, \(%rdi\)
**	movups	%xmm0, -16\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L5:
**	cmpl	\$64, %edx
**	jnb	.L14
**	movl	%edx, %edx
**	movdqu	\(%rsi\), %xmm3
**	movdqu	16\(%rsi\), %xmm2
**	addq	%rdx, %rsi
**	movdqu	-16\(%rsi\), %xmm1
**	movdqu	-32\(%rsi\), %xmm0
**	movups	%xmm3, \(%rdi\)
**	movups	%xmm2, 16\(%rdi\)
**	movups	%xmm1, -16\(%rdi,%rdx\)
**	movups	%xmm0, -32\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L6:
**	movl	%edx, %edx
**	movq	\(%rsi\), %rcx
**	movq	-8\(%rsi,%rdx\), %rax
**	movq	%rcx, \(%rdi\)
**	movq	%rax, -8\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L14:
**	movl	%edx, %edx
**	movdqu	\(%rsi\), %xmm7
**	movdqu	16\(%rsi\), %xmm6
**	movdqu	32\(%rsi\), %xmm5
**	movdqu	48\(%rsi\), %xmm4
**	addq	%rdx, %rsi
**	movdqu	-16\(%rsi\), %xmm3
**	movdqu	-32\(%rsi\), %xmm2
**	movdqu	-48\(%rsi\), %xmm1
**	movdqu	-64\(%rsi\), %xmm0
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
**.L7:
**	movl	%edx, %edx
**	movl	\(%rsi\), %ecx
**	movl	-4\(%rsi,%rdx\), %eax
**	movl	%ecx, \(%rdi\)
**	movl	%eax, -4\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L8:
**	movl	%edx, %edx
**	movzwl	\(%rsi\), %ecx
**	movzwl	-2\(%rsi,%rdx\), %eax
**	movw	%cx, \(%rdi\)
**	movw	%ax, -2\(%rdi,%rdx\)
**	ret
**	.cfi_endproc
**...
*/

void
gcc_memmove (void *a, void *b, __SIZE_TYPE__ n)
{
  if (n <= 66)
    __builtin_memmove (a, b, n);
}
