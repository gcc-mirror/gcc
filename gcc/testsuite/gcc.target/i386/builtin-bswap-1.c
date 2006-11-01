/* { dg-do compile } */
/* { dg-options "-march=i486" } */
/* { dg-final { scan-assembler "bswap" } } */

int foo (int a)
{
  int b;

  b = __builtin_bswap (a);

  return b;
}
