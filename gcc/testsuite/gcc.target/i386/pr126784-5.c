/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -march=x86-64" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**func:
**.LFB0:
**	.cfi_startproc
**	movl	\$34, var\(%rip\)
**	ret
**...
*/

extern unsigned int var;

void
func (void)
{
  var = (float) 34;
}
