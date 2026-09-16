/* { dg-lto-options {{ -O3 -flto -fdump-tree-optimized }} } */
/* { dg-lto-do link } */

struct S
{
  int x;
};

int bar (struct S);

void
foo (struct S s)
{
  int c = bar (s);
  for (int i = 0; i < c; i++)
    __builtin_abort ();
}

int
main ()
{
  int x;
  __builtin_scanf ("%d", &x);
  foo ({x});
  return 0;
}

/* { dg-final { scan-ltrans-tree-dump-not "__builtin_abort" "optimized" } } */
