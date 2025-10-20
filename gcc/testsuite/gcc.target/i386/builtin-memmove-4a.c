/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic -minline-all-stringops" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { lp64 } } {^\t?\.} } } */

/*
**gcc_memmove_xmm:
**.LFB0:
**	.cfi_startproc
**	cmpq	\$32, %rdx
**	ja	.L13
**.L1:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L13:
**	movq	%rdi, %rcx
**	movq	%rsi, %rax
**	cmpq	\$128, %rdx
**	jbe	.L14
**	movq	%rdx, %rsi
**	cmpq	%rdi, %rax
**	jb	.L7
**	je	.L1
**	movdqu	-16\(%rax,%rdx\), %xmm7
**	movdqu	-32\(%rax,%rdx\), %xmm6
**	movdqu	-48\(%rax,%rdx\), %xmm5
**	movdqu	-64\(%rax,%rdx\), %xmm4
**.L8:
**	movdqu	\(%rax\), %xmm3
**	subq	\$64, %rsi
**	addq	\$64, %rcx
**	addq	\$64, %rax
**	movdqu	-48\(%rax\), %xmm2
**	movdqu	-32\(%rax\), %xmm1
**	movdqu	-16\(%rax\), %xmm0
**	movups	%xmm3, -64\(%rcx\)
**	movups	%xmm2, -48\(%rcx\)
**	movups	%xmm1, -32\(%rcx\)
**	movups	%xmm0, -16\(%rcx\)
**	cmpq	\$64, %rsi
**	ja	.L8
**	movups	%xmm7, -16\(%rdi,%rdx\)
**	movups	%xmm6, -32\(%rdi,%rdx\)
**	movups	%xmm5, -48\(%rdi,%rdx\)
**	movups	%xmm4, -64\(%rdi,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L14:
**	cmpq	\$64, %rdx
**	jb	.L6
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
**	movdqu	\(%rax\), %xmm3
**	movdqu	16\(%rax\), %xmm2
**	leaq	\(%rdi,%rdx\), %rcx
**	movdqu	32\(%rax\), %xmm1
**	movdqu	48\(%rax\), %xmm0
**	addq	%rdx, %rax
**.L9:
**	movdqu	-16\(%rax\), %xmm7
**	movdqu	-32\(%rax\), %xmm6
**	subq	\$64, %rsi
**	subq	\$64, %rcx
**	movdqu	-48\(%rax\), %xmm5
**	movdqu	-64\(%rax\), %xmm4
**	subq	\$64, %rax
**	movups	%xmm7, 48\(%rcx\)
**	movups	%xmm6, 32\(%rcx\)
**	movups	%xmm5, 16\(%rcx\)
**	movups	%xmm4, \(%rcx\)
**	cmpq	\$64, %rsi
**	ja	.L9
**	movups	%xmm3, \(%rdi\)
**	movups	%xmm2, 16\(%rdi\)
**	movups	%xmm1, 32\(%rdi\)
**	movups	%xmm0, 48\(%rdi\)
**	ret
**	.cfi_endproc
**...
*/

#ifndef gcc_memmove
#define gcc_memmove gcc_memmove_xmm
#endif

void
gcc_memmove (void *a, void *b, __SIZE_TYPE__ n)
{
  if (n > 32)
    __builtin_memmove (a, b, n);
}
