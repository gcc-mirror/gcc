/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	pand	.LC[0-9]+\(%rip\), %xmm0
**	ret
**...
*/

__float128
foo (__float128 x)
{
  return __builtin_copysignq (x, 0.0);
}

/* { dg-final { scan-assembler-times ".long	-1" 3 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times ".long	2147483647" 1 { target { ! ia32 } } } } */
