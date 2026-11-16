/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-require-effective-target le } */

typedef __UINT64_TYPE__ u64;

static inline u64
g(u64 *restrict p, unsigned char *q, int c)
{
  *q = 1;                          /* byte 0, not through p                */
  ((unsigned char *)p)[4] = 7;     /* byte 4, through p                     */
  u64 r = 0;
  if (c) r = *p;                   /* not executed (c == 0): no restrict UB */
  return r;
}

__attribute__((noinline)) u64
f(u64 *x, unsigned char *y, int c)
{
  *x = 5;
  u64 r = g(x, y, c);
  return r + *x;                   /* outside g: must see *y = 1            */
}

int main()
{
  u64 a;
  u64 r = f(&a, (unsigned char *)&a, 0);
  if (r != 0x700000001ULL)
    __builtin_abort ();
  return 0;
}
