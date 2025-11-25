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
**	movl	\$512, %edx
**	cmpq	%rdi, %rsi
**	jb	.L5
**	je	.L1
**	movdqu	496\(%rsi\), %xmm7
**	movdqu	480\(%rsi\), %xmm6
**	movdqu	464\(%rsi\), %xmm5
**	movdqu	448\(%rsi\), %xmm4
**.L6:
**	movdqu	\(%rsi\), %xmm3
**	movdqu	16\(%rsi\), %xmm2
**	subl	\$64, %edx
**	addq	\$64, %rax
**	movdqu	32\(%rsi\), %xmm1
**	movdqu	48\(%rsi\), %xmm0
**	addq	\$64, %rsi
**	movups	%xmm3, -64\(%rax\)
**	movups	%xmm2, -48\(%rax\)
**	movups	%xmm1, -32\(%rax\)
**	movups	%xmm0, -16\(%rax\)
**	cmpl	\$64, %edx
**	ja	.L6
**	movups	%xmm7, 496\(%rdi\)
**	movups	%xmm6, 480\(%rdi\)
**	movups	%xmm5, 464\(%rdi\)
**	movups	%xmm4, 448\(%rdi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L5:
**	movdqu	\(%rsi\), %xmm7
**	movdqu	16\(%rsi\), %xmm6
**	leaq	512\(%rdi\), %rax
**	addq	\$512, %rsi
**	movdqu	-480\(%rsi\), %xmm5
**	movdqu	-464\(%rsi\), %xmm4
**.L7:
**	movdqu	-16\(%rsi\), %xmm3
**	subl	\$64, %edx
**	subq	\$64, %rax
**	subq	\$64, %rsi
**	movdqu	32\(%rsi\), %xmm2
**	movdqu	16\(%rsi\), %xmm1
**	movdqu	\(%rsi\), %xmm0
**	movups	%xmm3, 48\(%rax\)
**	movups	%xmm2, 32\(%rax\)
**	movups	%xmm1, 16\(%rax\)
**	movups	%xmm0, \(%rax\)
**	cmpl	\$64, %edx
**	ja	.L7
**	movups	%xmm7, \(%rdi\)
**	movups	%xmm6, 16\(%rdi\)
**	movups	%xmm5, 32\(%rdi\)
**	movups	%xmm4, 48\(%rdi\)
**.L1:
**	ret
**	.cfi_endproc
**...
*/

#ifndef gcc_memmove
#define gcc_memmove gcc_memmove_xmm
#endif

void
gcc_memmove (void *a, void *b)
{
  __builtin_memmove (a, b, 512);
}
