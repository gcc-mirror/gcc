/* { dg-do run } */
/* { dg-options "-fno-tree-pta" } */

static void __attribute__((noinline))
foo (int *i, int n)
{
  *i = n;
}

int
main (void)
{
  int i = 0;
  foo (&i, 1);

  if (i != 1)
    __builtin_abort ();

  return 0;
}

