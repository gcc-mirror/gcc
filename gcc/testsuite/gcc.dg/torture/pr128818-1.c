/* { dg-do run } */

typedef __UINT64_TYPE__ u64;

static inline u64
g(u64 *restrict p, u64 *y, int k, int c)
{
  *y = 2;
  p[k] = 0;
  u64 r = 0;
  if (c) r = *p;
  return r;
}

__attribute__((noinline)) u64
f(u64 *x, u64 *y, int k, int c)
{
  __builtin_memset(x, 0, 16);
  u64 r = g(x, y, k, c);
  return r + *x;
}

int main()
{
  u64 a[2];
  u64 r = f(&a[0], &a[0], 1, 0);
  if (r != 2)
    __builtin_abort ();
  return 0;
}
