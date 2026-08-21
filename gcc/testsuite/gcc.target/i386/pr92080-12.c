/* PR tree-optimization/112104  */
/* { dg-do compile } */
/* { dg-additional-options "-O3 -mno-mmx -march=icelake-server" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { *-*-linux* && { ! ia32 } } } {^\t?\.} } } */

/*
**f:
**.LFB0:
**	.cfi_startproc
**	testl	%edi, %edi
**	je	.L3
**	movl	%edi, %eax
**	andl	\$1, %edi
**	negl	%edi
**	negl	%eax
**	andl	%esi, %edi
**	xorb	%dil, a\(%rip\)
**	ret
**...
**.L3:
**	xorl	%eax, %eax
**	ret
**	.cfi_endproc
**...
*/

signed char a;
signed char f (int i, int j)
{
  signed char c;
  while (i != 0)
  {
    a ^= j;
    ++c;
    ++i;
  }
  return c;
}
