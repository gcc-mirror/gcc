/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic -minline-all-stringops" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { lp64 } } {^\t?\.} } } */

/*
**gcc_memmove:
**.LFB0:
**	.cfi_startproc
**	cmpq	\$3, %rdx
**	jbe	.L7
**.L1:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L7:
**	cmpl	\$2, %edx
**	jnb	.L8
**	cmpl	\$1, %edx
**	jb	.L1
**	movzbl	\(%rsi\), %eax
**	movb	%al, \(%rdi\)
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
  if (n < 4)
    __builtin_memmove (a, b, n);
}
