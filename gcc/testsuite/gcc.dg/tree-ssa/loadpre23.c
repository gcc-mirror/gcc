/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

struct {
  int a;
  int large[100];
} x;

int foo(int argc)
{
  int b;
  int c;
  int i;
  int d, e;

  for (i = 0; i < argc; i++)
    {
      e = x.a;
      x.a = 9;
    }
  return d + e;
}

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"  } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
