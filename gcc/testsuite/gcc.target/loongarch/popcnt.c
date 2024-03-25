/* { dg-do compile } */
/* { dg-options "-O2 -mlsx" } */
/* { dg-final { scan-assembler-not {popcount} } } */
/* { dg-final { scan-assembler-times "vpcnt.d" 2 { target { loongarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times "vpcnt.w" 4 { target { loongarch64*-*-* } } } } */

int
foo (int x)
{
  return __builtin_popcount (x);
}

long
foo1 (long x)
{
  return __builtin_popcountl (x);
}

long long
foo2 (long long x)
{
  return __builtin_popcountll (x);
}

int
foo3 (int *p)
{
  return __builtin_popcount (*p);
}

unsigned
foo4 (int x)
{
  return __builtin_popcount (x);
}

unsigned long
foo5 (int x)
{
  return __builtin_popcount (x);
}
