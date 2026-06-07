/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { lp64 } } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	movdqu	a\+20\(%rip\), %xmm5
**	movdqu	a\+36\(%rip\), %xmm4
**	movdqu	a\+52\(%rip\), %xmm3
**	movdqu	a\+68\(%rip\), %xmm2
**	movdqu	a\+84\(%rip\), %xmm1
**	movdqu	a\+100\(%rip\), %xmm0
**	movups	%xmm5, a\+24\(%rip\)
**	movq	a\+116\(%rip\), %rax
**	movdqu	a\+4\(%rip\), %xmm6
**	movups	%xmm4, a\+40\(%rip\)
**	movl	%edi, a\+4\(%rip\)
**	movq	%rax, a\+120\(%rip\)
**	movups	%xmm6, a\+8\(%rip\)
**	movups	%xmm3, a\+56\(%rip\)
**	movups	%xmm2, a\+72\(%rip\)
**	movups	%xmm1, a\+88\(%rip\)
**	movups	%xmm0, a\+104\(%rip\)
**	ret
**	.cfi_endproc
**...
*/

#define N 32

int a[N];

void
foo (int x)
{
  __builtin_memmove (a + 2, a + 1, sizeof a - 2 * sizeof *a);
  a[1] = x;
}
