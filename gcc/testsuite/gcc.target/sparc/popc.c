/* { dg-do compile } */
/* { dg-options "-mcpu=niagara2" } */
int test_popcount(int a)
{
  return __builtin_popcount(a);
}

long test_popcountl(long a)
{
  return __builtin_popcountl(a);
}

long long test_popcountll(long long a)
{
  return __builtin_popcountll(a);
}

/* { dg-final { scan-assembler-times "popc\t%" 3 } } */
