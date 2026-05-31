/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse -mmemcpy-strategy=unrolled_loop:256:noalign,libcall:-1:noalign" } */
/* { dg-final { check-function-bodies "**" "*#" "" { target { lp64 && { ! *-*-darwin* } } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target { lp64 && *-*-darwin* } } {^\t?\.} } } */

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
*#

*Dfoo:
*D	movq	221\(%rsi\), %rax
*D	xorl	%edx, %edx
*D	movq	%rax, 221\(%rdi\)
*D	movq	229\(%rsi\), %rax
*D	movq	%rax, 229\(%rdi\)
*D	movq	237\(%rsi\), %rax
*D	movq	%rax, 237\(%rdi\)
*D	movq	245\(%rsi\), %rax
*D	movq	%rax, 245\(%rdi\)
*D.L[0-9]+:
*D	movl	%edx, %eax
*D	addl	\$32, %edx
*D	movq	\(%rsi,%rax\), %r10
*D	cmpl	\$224, %edx
*D	movq	8\(%rsi,%rax\), %r9
*D	movq	16\(%rsi,%rax\), %r8
*D	movq	24\(%rsi,%rax\), %rcx
*D	movq	%r10, \(%rdi,%rax\)
*D	movq	%r9, 8\(%rdi,%rax\)
*D	movq	%r8, 16\(%rdi,%rax\)
*D	movq	%rcx, 24\(%rdi,%rax\)
*D	jb	.L[0-9]+
*D	ret
*D...
*E
*/

void
foo (char *dest, char *src)
{
  __builtin_memcpy (dest, src, 253);
}

/* { dg-final { scan-assembler-not "rep mov" } } */
