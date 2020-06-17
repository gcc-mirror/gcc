/* { dg-do compile } */
/* { dg-options "-O2 -march=core-avx2" } */
/* { dg-final { scan-assembler "popcnt" } } */
/* { dg-final { scan-assembler "and" } } */

int foo(unsigned int x)
{
  return __builtin_parity(x);
}
