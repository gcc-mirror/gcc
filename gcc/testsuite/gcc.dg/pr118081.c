/* PR tree-optimization/118081 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-vrp -fno-expensive-optimizations" } */

int a, b;

int
foo (int f)
{
  return f ? f || 0 : f;
}

void
bar (void)
{
  b = a ? a : 1;
  int i = foo (1 ^ b);
  signed char h = i - 8;
  if (h)
    return;
  __builtin_abort ();
}

int
main ()
{
  bar ();
}
