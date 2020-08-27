/* { dg-do run } */
/* { dg-additional-options "-fno-tree-pta" } */

__attribute__((noipa)) void
bar (void)
{
  volatile int v = 1;
  if (v)
    __builtin_abort ();
}

__attribute__((noipa)) void
baz (void)
{
}

__attribute__((noipa)) void
foo (int n, double *p, double *x)
{
  if (n < 10 && p != 0)
    for (int i = 0; i < 10; i++)
      if (x[0] < p[i])
        x[i] = 0;
  if (p != 0)
    bar ();
  else
    baz ();
}

int
main ()
{
  double arr[10];
  foo (1000, 0, arr);
  return 0;
}
