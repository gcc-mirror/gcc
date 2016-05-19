/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats -fno-tree-loop-im" } */

int a;

int foo(int argc)
{
  int i;
  int e;

  for (i = 0; i < argc; i++)
    {
      e = a;
      a = 9;
    }
  return e;
}

/* We will move the load of a out of the loop.  */

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre" } } */
