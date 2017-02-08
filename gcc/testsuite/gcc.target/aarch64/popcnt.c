/* { dg-do compile } */
/* { dg-options "-O2" } */

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

/* { dg-final { scan-assembler-not "popcount" } } */
/* { dg-final { scan-assembler-times "cnt\t" 3 } } */
