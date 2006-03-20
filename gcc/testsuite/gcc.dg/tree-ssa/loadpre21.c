/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
typedef int type[2];
int main(type *a, int argc)
{
  int b;
  int i;
  int d, e;

  /* Should be able to hoist this out of the loop.  */
  for (i = 0; i < argc; i++)
    {
      e = (*a)[0];
    }
  return d + e;
}

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
