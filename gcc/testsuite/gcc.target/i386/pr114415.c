/* { dg-do run  { target int128 } } */
/* { dg-options "-Oz -fno-dce -fno-forward-propagate -flive-range-shrinkage -fweb -Wno-psabi" } */

typedef unsigned V __attribute__((vector_size (64)));
typedef unsigned __int128 W __attribute__((vector_size (64)));
__int128 i;
V v;
W w;

W
bar2 (V, W c)
{
  v |= (V){c[3]};
  i = 0;
  return c;
}

W
bar1 (W, W d)
{
  v[0] %= 2;
  return d;
}

W
bar0 (V a, W b)
{
  W t = bar2 ((V) { }, w);
  (void)bar2 ((V){b[0] - a[0]}, (W){});
  b += bar1 (t, (W){1});
  a[5] &= __builtin_mul_overflow_p (a[0], i, 0u);
  return (W) a + b;
}

int
main ()
{
  W x = bar0 ((V) { }, (W) { });
  if (x[0] != 1)
    __builtin_abort();
  if (x[1])
    __builtin_abort();
  if (x[2])
    __builtin_abort();
  if (x[3])
    __builtin_abort();
}
