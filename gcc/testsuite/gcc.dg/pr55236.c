/* PR tree-optimization/55236 */
/* { dg-do run } */
/* { dg-options "-O2 -fwrapv" } */

extern void abort ();

__attribute__((noinline, noclone)) void
foo (int i)
{
  if (i > 0)
    abort ();
  i = -i;
  if (i < 0)
    return;
  abort ();
}

__attribute__((noinline, noclone)) void
bar (int i)
{
  if (i > 0 || (-i) >= 0)
    abort ();
}

int
main ()
{
  foo (-__INT_MAX__ - 1);
  bar (-__INT_MAX__ - 1);
  return 0;
}
