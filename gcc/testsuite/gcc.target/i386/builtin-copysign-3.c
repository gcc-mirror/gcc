/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	orps	.LC[0-9]+\(%rip\), %xmm0
**	ret
**...
*/

float
foo (float x)
{
  return __builtin_copysignf (x, -3.0);
}

/* { dg-final { scan-assembler-times ".long	0" 3 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times ".long	-2147483648" 1 { target { ! ia32 } } } } */
