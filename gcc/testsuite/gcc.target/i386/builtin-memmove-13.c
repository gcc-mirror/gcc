/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { lp64 } } {^\t?\.} } } */

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
*/

char a[8] = "12345678";

void
foo (void)
{
  __builtin_memmove (a, a + 3, sizeof a - 3);
}
