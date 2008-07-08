/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

int a;

int foo(int argc)
{
  int b;
  int c;
  int i;
  int d, e;

  for (i = 0; i < argc; i++)
    {
      e = a;
      a = 9;
    }
  return d + e;
}

/* We will move the load of a out of the loop.  */

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre" } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
