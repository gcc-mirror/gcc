/* { dg-do compile } */
/* { dg-options "-O2 -march=c64x+" } */
/* { dg-final { scan-assembler-not "call" } } */

int foo (int x)
{
  return __builtin_ffsl (x);
}

int bar (int x)
{
  return __builtin_clzl (x);
}

int baz (int x)
{
  return __builtin_ctzl (x);
}
