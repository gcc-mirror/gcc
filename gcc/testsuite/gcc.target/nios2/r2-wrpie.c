/* { dg-do compile } */
/* { dg-options "-O2 -march=r2" } */
/* { dg-final { scan-assembler "wrpie" } } */

int
foo (int a)
{
  int b;

  b = __builtin_wrpie (a);
  a = __builtin_wrpie (b);

  return a + b;
}
