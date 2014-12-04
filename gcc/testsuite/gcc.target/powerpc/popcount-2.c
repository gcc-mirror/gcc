/* { dg-do compile { target { ilp32 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7" } */
/* { dg-final { scan-assembler "popcntw" } } */

int foo(int x)
{
  return __builtin_popcount(x);
}
