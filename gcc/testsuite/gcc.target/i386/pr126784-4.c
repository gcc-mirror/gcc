/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -march=x86-64" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**func:
**.LFB0:
**	.cfi_startproc
**	movdqa	.LC0\(%rip\), %xmm0
**	movups	%xmm0, var\(%rip\)
**	movdqa	.LC1\(%rip\), %xmm0
**	movups	%xmm0, var\+16\(%rip\)
**	ret
**...
*/

extern unsigned int var[8];

void
func (void)
{
  int i;
  for (i = 0; i < 8; i++)
    var[i] = (float) i;
}
