/* { dg-do compile { target { ilp32 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-options "-O2 -mcpu=power5" } */
/* { dg-final { scan-assembler "popcntb" } } */
/* { dg-final { scan-assembler-not "mullw" } } */

int foo(int x)
{
  return __builtin_parity(x);
}
