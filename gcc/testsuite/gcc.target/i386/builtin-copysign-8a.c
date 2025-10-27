/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	pand	.LC[0-9]+\(%rip\), %xmm0
**	pand	.LC[0-9]+\(%rip\), %xmm1
**	por	%xmm1, %xmm0
**	ret
**...
*/

__float128
foo (__float128 x, __float128 y)
{
  return __builtin_copysignq (x, y);
}
