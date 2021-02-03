/* PR tree-optimization/96271 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=intel -msse2 -masm=att" } */
/* { dg-final { scan-assembler "movq\t%xmm0, %r" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movq\t%xmm1, %r" { target { ! ia32 } } } } */

int
foo (double a, double b)
{
  return __builtin_memcmp (&a, &b, sizeof (double)) == 0;
}
