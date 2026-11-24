/* PR tree-optimization/128904 */
/* { dg-do run } */
/* { dg-additional-options "-ftree-vectorize" } */

__attribute__((noipa)) unsigned
f (unsigned a, unsigned c)
{
  unsigned short acc = a;
  c &= 3;
  for (int i = 0; i < 31; i++)
    acc |= __builtin_ctzg (c, -1) & 1;
  return acc;
}

__attribute__((noipa)) unsigned
g (unsigned a, unsigned c)
{
  unsigned short acc = a;
  c &= 3;
  for (int i = 0; i < 31; i++)
    acc |= __builtin_clzg (c, -1) & 1;
  return acc;
}

__attribute__((noipa)) unsigned long long
h (unsigned long long a, unsigned long long c)
{
  unsigned long long acc = a;
  c &= 3;
  for (int i = 0; i < 31; i++)
    acc |= (__builtin_ctzg (c, -1) & 1) | (__builtin_clzg (c, -1) & 2);
  return acc;
}

/* The guarded call is invariant in the loop.  */
__attribute__((noipa)) int
k (int *p, unsigned c, int n)
{
  int s = 0;
  for (int i = 0; i < n; i++)
    s += p[i] + (c ? __builtin_ctz (c) : 7);
  return s;
}

int
main (void)
{
  int a[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
  if (f (0, 0) != 1 || f (0, 1) != 0 || f (0, 2) != 1 || f (0, 3) != 0)
    __builtin_abort ();
  if (g (0, 0) != 1 || g (0, 1) != 1 || g (0, 2) != 0 || g (0, 3) != 0)
    __builtin_abort ();
  if (h (0, 0) != 3 || h (0, 1) != 2 || h (0, 2) != 3 || h (0, 3) != 2)
    __builtin_abort ();
  if (k (a, 0, 8) != 64 || k (a, 4, 8) != 24)
    __builtin_abort ();
  return 0;
}
