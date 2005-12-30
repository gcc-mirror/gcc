/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
int main(int *a, int argc)
{
  int b;
  int c;
  int i;
  int d, e;

  /* With smarter load PRE, we'd be able to recompute the value at the 
     kill point.  arguably not worth it.  */
  for (i = 0; i < argc; i++)
    {
      e = *a;
      *a = 9;
    }
  return d + e;
}

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
