/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic -mno-sse" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	movq	221\(%rsi\), %rax
**	xorl	%edx, %edx
**	movq	%rax, 221\(%rdi\)
**	movq	229\(%rsi\), %rax
**	movq	%rax, 229\(%rdi\)
**	movq	237\(%rsi\), %rax
**	movq	%rax, 237\(%rdi\)
**	movq	245\(%rsi\), %rax
**	movq	%rax, 245\(%rdi\)
**.L[0-9]+:
**	movl	%edx, %eax
**	addl	\$32, %edx
**	movq	\(%rsi,%rax\), %r10
**	movq	8\(%rsi,%rax\), %r9
**	movq	16\(%rsi,%rax\), %r8
**	movq	24\(%rsi,%rax\), %rcx
**	movq	%r10, \(%rdi,%rax\)
**	movq	%r9, 8\(%rdi,%rax\)
**	movq	%r8, 16\(%rdi,%rax\)
**	movq	%rcx, 24\(%rdi,%rax\)
**	cmpl	\$224, %edx
**	jb	.L[0-9]+
**	ret
**...
*/

void
foo (char *dest, char *src)
{
  __builtin_memcpy (dest, src, 253);
}

/* { dg-final { scan-assembler-not "rep mov" } } */
