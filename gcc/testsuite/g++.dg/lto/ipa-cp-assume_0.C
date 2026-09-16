/* { dg-lto-options {{ -O3 -flto -fdump-tree-optimized }} } */
/* { dg-lto-do link } */

/* The point of this test is to propagate the constant into the assume body in
   the other file, infer the value range for 'x', evaluate bar to 0, meaning
   that the loop will never run and the abort would never be executed.  */

int bar (int, int);

void
foo (int x, int y)
{
  int c = bar (x, y);
  for (int i = 0; i < c; i++)
    __builtin_abort ();
}

int
main ()
{
  int x;
  __builtin_scanf ("%d", &x);
  foo (x, 67);
  return 0;
}

/* { dg-final { scan-ltrans-tree-dump-not "__builtin_abort" "optimized" } } */
