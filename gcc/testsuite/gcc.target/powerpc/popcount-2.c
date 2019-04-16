/* { dg-do compile { target { ilp32 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7" } */
/* { dg-final { scan-assembler "popcntw" } } */

int foo(int x)
{
  return __builtin_popcount(x);
}
