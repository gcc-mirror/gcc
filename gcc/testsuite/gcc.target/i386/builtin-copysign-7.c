/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	pand	.LC[0-9]+\(%rip\), %xmm0
**	por	.LC[0-9]+\(%rip\), %xmm0
**	ret
**...
*/

__float128
foo (__float128 x)
{
  return __builtin_copysignq (3.4, x);
}
