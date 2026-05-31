/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "*#" "" { target { lp64 && { ! *-*-darwin* } } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target { lp64 && *-*-darwin* } } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	movl	a\+3\(%rip\), %eax
**	movl	%eax, a\(%rip\)
**	movzbl	a\+7\(%rip\), %eax
**	movb	%al, a\+4\(%rip\)
**	ret
**	.cfi_endproc
**...
*#

*Dfoo:
*D	movl	3\+_a\(%rip\), %eax
*D	movl	%eax, _a\(%rip\)
*D	movzbl	7\+_a\(%rip\), %eax
*D	movb	%al, 4\+_a\(%rip\)
*D	ret
*D...
*E
*/

char a[8] = "12345678";

void
foo (void)
{
  __builtin_memmove (a, a + 3, sizeof a - 3);
}
