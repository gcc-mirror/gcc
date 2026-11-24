/* PR tree-optimization/128904 */
/* { dg-do run } */
/* { dg-require-effective-target int128 } */

typedef unsigned __int128 T;

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

int
main (void)
{
  if (f (0, -1) != -1 || g (0, -1) != -1 || h (0) != 0)
    __builtin_abort ();
  if (f (1, -1) != 127 || g (2, -1) != 1 || h (2) != 126 + 1)
    __builtin_abort ();
  return 0;
}
