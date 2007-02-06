/* { dg-do compile { target { ilp32 } } } */
/* { dg-options "-O2 -mcpu=power6" } */
/* { dg-final { scan-assembler "popcntb" } } */
/* { dg-final { scan-assembler-not "mullw" } } */

int foo(int x)
{
  return __builtin_popcount(x);
}
