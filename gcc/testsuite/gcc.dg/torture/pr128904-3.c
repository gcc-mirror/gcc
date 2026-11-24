/* PR tree-optimization/128904 */
/* { dg-do run } */
/* { dg-require-effective-target bitint } */

typedef unsigned _BitInt(256) T;

__attribute__((noipa)) int
f (T x, int d)
{
  return __builtin_clzg (x, d);
}

__attribute__((noipa)) int
g (T x, int d)
{
  return __builtin_ctzg (x, d);
}

__attribute__((noipa)) int
h (T x)
{
  return (x ? __builtin_clzg (x) : 0) + (x ? __builtin_ctzg (x) : 0);
}

/* The guarded calls are invariant in the loop.  */
__attribute__((noipa)) int
k (T x, int *p, int n)
{
  int s = 0;
  for (int i = 0; i < n; i++)
    s += p[i] + (x ? __builtin_clzg (x) : 7) + (x ? __builtin_ctzg (x) : 9);
  return s;
}

int
main (void)
{
  int a[4] = { 1, 1, 1, 1 };
  if (f (0, -1) != -1 || g (0, -1) != -1 || h (0) != 0)
    __builtin_abort ();
  if (f (1, -1) != 255 || g (2, -1) != 1 || h (2) != 254 + 1)
    __builtin_abort ();
  if (k (0, a, 4) != 4 + 4 * 16 || k (4, a, 4) != 4 + 4 * (253 + 2))
    __builtin_abort ();
  return 0;
}
