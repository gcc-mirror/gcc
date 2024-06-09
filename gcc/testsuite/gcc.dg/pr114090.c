/* PR tree-optimization/114090 */
/* { dg-do run } */
/* { dg-options "-O2 -fwrapv" } */

__attribute__((noipa)) int
foo (int x)
{
  int w = (x >= 0 ? x : 0);
  int y = -x;
  int z = (y >= 0 ? y : 0);
  return w + z;
}

__attribute__((noipa)) int
bar (int x)
{
  int w = (x >= 0 ? x : 0);
  int z = (x <= 0 ? -x : 0);
  return w + z;
}

__attribute__((noipa)) int
baz (int x)
{
  return x <= 0 ? -x : 0;
}

int
main ()
{
  int v = -__INT_MAX__ - 1;
  if (foo (v) != 0)
    __builtin_abort ();
  if (bar (v) != v)
    __builtin_abort ();
  if (baz (v) != v)
    __builtin_abort ();
}
