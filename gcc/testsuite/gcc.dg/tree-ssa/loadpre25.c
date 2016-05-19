/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats -fno-tree-loop-im" } */
struct X { int i; };
int foo(struct X *a, int argc)
{
  int i;
  int e;

  for (i = 0; i < argc; i++)
    {
      e = a->i;
      a->i = 9;
    }
  return e;
}

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"  } } */
