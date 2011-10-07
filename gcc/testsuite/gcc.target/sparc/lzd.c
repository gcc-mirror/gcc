/* { dg-do compile { target { vis3 } } } */
/* { dg-options "-mcpu=niagara3" } */
int test_clz(int a)
{
  return __builtin_clz(a);
}

long test_clzl(long a)
{
  return __builtin_clzl(a);
}

long long test_clzll(long long a)
{
  return __builtin_clzll(a);
}

/* { dg-final { scan-assembler-times "lzd\t%" 3 } } */
