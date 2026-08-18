/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**func:
**.LFB0:
**	.cfi_startproc
**	cmpl	%esi, %edi
**	movl	%esi, %eax
**	cmovle	%edi, %eax
**	movslq	%eax, %rcx
**	movq	%rcx, \(%[er]dx\)
**	ret
**...
*/

int
func (int a, int b, long long int *p)
{
  long long int x = a;
  long long int y = b;
  long long int z = x < y ? x : y;
  *p = z;
  return z;
}
