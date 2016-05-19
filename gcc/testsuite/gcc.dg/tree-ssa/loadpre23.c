/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats -fno-tree-loop-im" } */

struct {
  int a;
  int large[100];
} x;

int foo(int argc)
{
  int c;
  int i;
  int e;

  for (i = 0; i < argc; i++)
    {
      e = x.a;
      x.a = 9;
    }
  return e;
}

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"  } } */
