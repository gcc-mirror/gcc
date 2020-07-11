/* { dg-do compile } */
/* { dg-options "-O2 -march=core-avx2 -mno-popcnt" } */
/* { dg-final { scan-assembler-not "shr" } } */

int foo(unsigned short x)
{
  return __builtin_parity(x);
}

int bar(unsigned short x)
{
  return __builtin_parityll(x);
}
