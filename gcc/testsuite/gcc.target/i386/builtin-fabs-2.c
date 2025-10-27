/* PR target/99930 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**foo:
**...
**	ja	.L[0-9]+
**	movss	4\(%(e|r)di\), %xmm1
**	orps	%xmm1, %xmm0
**	comiss	%xmm0, %xmm2
**	seta	%al
**	ret
**...
*/


bool
foo (float n[2], float m)
{
  for (int i = 0; i < 2; i++)
    if (m > -__builtin_fabsf (n[i]))
      return true;
  return false;
}
