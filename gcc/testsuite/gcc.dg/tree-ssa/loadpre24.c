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

/* PRE of globals doesn't work.  */

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
