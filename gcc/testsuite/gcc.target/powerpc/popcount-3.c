/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-options "-O2 -mcpu=power7 -m64" } */
/* { dg-final { scan-assembler "popcntd" } } */

long foo(int x)
{
  return __builtin_popcountl(x);
}
